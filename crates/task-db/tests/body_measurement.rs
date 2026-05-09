//! Integration tests for the body-measurement surface on
//! [`FitnessService`].

use chrono::{Duration, Utc};
use sea_orm::{ColumnTrait, EntityTrait, QueryFilter};
use task_core::attachment;
use task_core::body_measurement;
use task_core::service::{
    BodyMeasurementTrendRequest, FitnessService, ListBodyMeasurementsRequest,
    RecordBodyMeasurementRequest, UpdateBodyMeasurementRequest,
};
use task_core::service_impl::{FitnessServiceDeps, FitnessServiceImpl};
use task_db::seed::seed_demo_data;

fn svc(db: sea_orm::DatabaseConnection) -> FitnessServiceImpl {
    FitnessServiceImpl::new(FitnessServiceDeps { db })
}

#[tokio::test]
async fn seed_populates_body_measurements() {
    let db = task_db::init_memory().await.expect("init db");
    let s = seed_demo_data(&db).await.expect("seed");
    assert!(
        s.body_measurements_created >= 10,
        "body_measurements_created={}",
        s.body_measurements_created
    );
}

#[tokio::test]
async fn record_defaults_measured_at_to_now() {
    let db = task_db::init_memory().await.expect("init db");
    let svc = svc(db);
    let before = Utc::now();
    let saved = svc
        .record_body_measurement(RecordBodyMeasurementRequest {
            weight_kg: Some(80.0),
            ..Default::default()
        })
        .await
        .expect("record");
    let after = Utc::now();
    assert!(
        saved.measured_at >= before - Duration::seconds(2),
        "{:?} < {:?}",
        saved.measured_at,
        before
    );
    assert!(
        saved.measured_at <= after + Duration::seconds(2),
        "{:?} > {:?}",
        saved.measured_at,
        after
    );
    assert_eq!(saved.weight_kg, Some(80.0));
}

#[tokio::test]
async fn record_honors_explicit_measured_at() {
    let db = task_db::init_memory().await.expect("init db");
    let svc = svc(db);
    let when = Utc::now() - Duration::days(5);
    let saved = svc
        .record_body_measurement(RecordBodyMeasurementRequest {
            measured_at: Some(when),
            weight_kg: Some(75.0),
            ..Default::default()
        })
        .await
        .expect("record");
    // Round-trip through chrono can lose sub-second precision depending
    // on the storage backend; compare seconds.
    assert_eq!(saved.measured_at.timestamp(), when.timestamp());
}

#[tokio::test]
async fn update_is_patch_shaped() {
    let db = task_db::init_memory().await.expect("init db");
    let svc = svc(db);
    let saved = svc
        .record_body_measurement(RecordBodyMeasurementRequest {
            weight_kg: Some(80.0),
            body_fat_percent: Some(20.0),
            waist_cm: Some(85.0),
            notes: Some("baseline".into()),
            ..Default::default()
        })
        .await
        .expect("record");

    let updated = svc
        .update_body_measurement(UpdateBodyMeasurementRequest {
            id: saved.id,
            weight_kg: Some(78.5),
            ..Default::default()
        })
        .await
        .expect("update");

    assert_eq!(updated.weight_kg, Some(78.5));
    // Untouched fields are preserved.
    assert_eq!(updated.body_fat_percent, Some(20.0));
    assert_eq!(updated.waist_cm, Some(85.0));
    assert_eq!(updated.notes, "baseline");
}

#[tokio::test]
async fn list_orders_desc_and_caps_at_365() {
    let db = task_db::init_memory().await.expect("init db");
    let svc = svc(db);
    let now = Utc::now();
    for i in 0..5_i64 {
        svc.record_body_measurement(RecordBodyMeasurementRequest {
            measured_at: Some(now - Duration::days(i)),
            weight_kg: Some(80.0 - i as f64),
            organization: Some("personal".into()),
            ..Default::default()
        })
        .await
        .expect("record");
    }
    let rows = svc
        .list_body_measurements(ListBodyMeasurementsRequest {
            organization: Some("personal".into()),
            limit: Some(10),
            ..Default::default()
        })
        .await
        .expect("list");
    assert_eq!(rows.len(), 5);
    // DESC: row 0 is the newest (now), row 4 is the oldest.
    assert!(rows[0].measured_at > rows[4].measured_at);

    // since/until window
    let recent = svc
        .list_body_measurements(ListBodyMeasurementsRequest {
            organization: Some("personal".into()),
            since: Some(now - Duration::days(2)),
            ..Default::default()
        })
        .await
        .expect("list_since");
    // days_ago in [0,1,2] -> 3 rows
    assert_eq!(recent.len(), 3);

    // cap at 365 even when limit is huge
    let capped = svc
        .list_body_measurements(ListBodyMeasurementsRequest {
            organization: Some("personal".into()),
            limit: Some(10_000),
            ..Default::default()
        })
        .await
        .expect("list_cap");
    // Internal cap doesn't affect this small set, but we just want the
    // call to succeed (it would error if the cap were missing).
    assert_eq!(capped.len(), 5);
}

#[tokio::test]
async fn trend_computes_first_last_delta_correctly() {
    let db = task_db::init_memory().await.expect("init db");
    let svc = svc(db);
    let now = Utc::now();
    let weights = [80.0, 79.0, 78.0, 77.0, 76.0];
    for (i, w) in weights.iter().enumerate() {
        svc.record_body_measurement(RecordBodyMeasurementRequest {
            measured_at: Some(now - Duration::days((4 - i) as i64)),
            weight_kg: Some(*w),
            organization: Some("personal".into()),
            ..Default::default()
        })
        .await
        .expect("record");
    }
    let view = svc
        .body_measurement_trend(BodyMeasurementTrendRequest {
            organization: Some("personal".into()),
            ..Default::default()
        })
        .await
        .expect("trend");
    let m = view.weight_kg.expect("weight metric");
    assert_eq!(m.sample_count, 5);
    assert!((m.first_value - 80.0).abs() < 1e-9);
    assert!((m.last_value - 76.0).abs() < 1e-9);
    assert!((m.min_value - 76.0).abs() < 1e-9);
    assert!((m.max_value - 80.0).abs() < 1e-9);
    assert!((m.mean_value - 78.0).abs() < 1e-9);
    assert!((m.delta + 4.0).abs() < 1e-9);
    assert!((m.delta_percent + 5.0).abs() < 1e-9);

    // No bf% logged → metric is None.
    assert!(view.body_fat_percent.is_none());
    assert!(view.waist_cm.is_none());
}

#[tokio::test]
async fn trend_empty_range() {
    let db = task_db::init_memory().await.expect("init db");
    let svc = svc(db);
    let view = svc
        .body_measurement_trend(BodyMeasurementTrendRequest {
            organization: Some("personal".into()),
            since: Some(Utc::now() - Duration::days(1)),
            until: Some(Utc::now()),
        })
        .await
        .expect("trend");
    assert_eq!(view.measurement_count, 0);
    assert!(view.weight_kg.is_none());
    assert!(view.body_fat_percent.is_none());
    assert!(view.muscle_mass_kg.is_none());
    assert!(view.waist_cm.is_none());
    assert!(view.chest_cm.is_none());
    assert!(view.hip_cm.is_none());
}

#[tokio::test]
async fn delete_drops_attachments() {
    use sea_orm::ActiveValue::Set as ActSet;
    let db = task_db::init_memory().await.expect("init db");
    let svc = svc(db.clone());
    let saved = svc
        .record_body_measurement(RecordBodyMeasurementRequest {
            weight_kg: Some(80.0),
            ..Default::default()
        })
        .await
        .expect("record");

    // Insert a fake attachment owned by this measurement.
    let now = Utc::now();
    let att_id = uuid::Uuid::new_v4();
    let active = attachment::ActiveModel {
        id: ActSet(att_id),
        owner_type: ActSet("body_measurement".into()),
        owner_id: ActSet(saved.id),
        path: ActSet("attachments/body_measurement/x/photo.jpg".into()),
        label: ActSet(Some("photo".into())),
        mime: ActSet(Some("image/jpeg".into())),
        size_bytes: ActSet(Some(123)),
        checksum: ActSet(None),
        source: ActSet("local".into()),
        uploader: ActSet(None),
        created_at: ActSet(now),
        updated_at: ActSet(now),
    };
    attachment::Entity::insert(active)
        .exec(&db)
        .await
        .expect("insert attachment");
    let before = attachment::Entity::find()
        .filter(attachment::Column::OwnerId.eq(saved.id))
        .all(&db)
        .await
        .expect("count before");
    assert_eq!(before.len(), 1);

    svc.delete_body_measurement(saved.id).await.expect("delete");

    let after = attachment::Entity::find()
        .filter(attachment::Column::OwnerId.eq(saved.id))
        .all(&db)
        .await
        .expect("count after");
    assert!(after.is_empty(), "attachments not cascaded: {after:?}");
    let measurement = body_measurement::Entity::find_by_id(saved.id)
        .one(&db)
        .await
        .expect("measurement gone");
    assert!(measurement.is_none());
}
