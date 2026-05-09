pub mod activity;
pub mod agent;
pub mod asset;
pub mod attachment;
pub mod body_measurement;
pub mod calendar_event;
pub mod capture;
pub mod client;
pub mod comment;
pub mod cookbook;
pub mod cookbook_recipe;
pub mod cooking_session;
pub mod cycle;
pub mod email;
pub mod exercise;
pub mod expense;
pub mod food;
pub mod food_log;
pub mod food_product;
pub mod glossary;
pub mod invoice;
pub mod location;
pub mod meal_plan;
pub mod module;
pub mod notification;
pub mod nutrition;
pub mod pantry;
pub mod people;
pub mod project;
pub mod property;
pub mod query;
pub mod reaction;
pub mod recipe;
pub mod recipe_ingredient;
pub mod recipe_step;
pub mod revenue;
pub mod routine;
pub mod routine_exercise;
pub mod rrule;
pub mod set_log;
pub mod shopping_list;
pub mod substitution;
pub mod task;
pub mod task_relation;
pub mod team;
pub mod track;
pub mod views;
pub mod workflows;
pub mod workout_session;

#[cfg(feature = "server")]
pub mod index;
#[cfg(feature = "server")]
pub mod integration;
#[cfg(feature = "server")]
pub mod provider;
#[cfg(feature = "server")]
pub mod recipe_import;
#[cfg(feature = "server")]
pub mod service;
#[cfg(feature = "server")]
pub mod service_impl;

#[cfg(feature = "caldav")]
pub mod caldav;

#[cfg(feature = "realtime")]
pub mod crdt;

pub use activity::Activity;
pub use agent::{
    AgentPlan, AgentPlanEdge, AgentPlanNode, AgentPlanNodeKind, AgentPlanNodeStatus,
    build_agent_plan,
};
pub use asset::{
    Asset, AssetBucket, AssetConflict, AssetCreateRequest, AssetFilter, AssetMaintenanceRecord,
    AssetMaintenanceRequest, AssetPatch, AssetRepairRequest, AssetRepairResponse, AssetReport,
    AssetReservationRecord, AssetReservationResponse, AssetReserveRequest, AssetStatus,
    format_asset_id,
};
pub use attachment::{Attachment, DEFAULT_ATTACHMENT_SOURCE, default_remote_path, label_from_path};
pub use body_measurement::{BodyMeasurement, BodyMeasurementApi};
pub use calendar_event::{CalendarEvent, CalendarEventStatus};
pub use capture::{CaptureInput, parse_capture};
pub use client::{Client, resolve_rate};
pub use comment::Comment;
pub use cookbook::{Cookbook, CookbookApi};
pub use cookbook_recipe::{CookbookRecipe, CookbookRecipeApi};
pub use cooking_session::{
    CookingSession, CookingSessionApi, CookingSessionStatus, StepState as CookingStepState,
};
pub use cycle::{Cycle, CycleStatus};
pub use email::EmailRef;
pub use exercise::{
    Exercise, ExerciseAliasList, ExerciseApi, ExerciseModality, ExerciseMuscleList,
};
pub use expense::{
    Expense, ExpenseBucket, ExpenseCreateRequest, ExpensePatch, ExpenseReport, ExpenseStatus,
    format_expense_id,
};
pub use food::{Food, FoodAliasList, FoodApi};
pub use food_log::{FoodLog, FoodLogApi};
pub use food_product::{FoodProduct, FoodProductApi};
pub use glossary::{
    GlossaryAliasList, GlossaryRelatedList, GlossaryTerm, GlossaryTermApi, ResolvedWikilink,
    WikilinkSpan, find_term_by_slug_or_alias as find_glossary_term, find_wikilinks,
    render_for_terminal as render_wikilinks_for_terminal, resolve_wikilinks,
};
pub use invoice::{Invoice, InvoiceLine, InvoiceStatus, Payment, format_invoice_id};
pub use location::{Location, Space, VenueDefault, render_location_body};
pub use meal_plan::{MealPlanEntry, MealPlanEntryApi, MealType};
pub use module::{Module, ModuleStatus};
pub use notification::Notification;
pub use nutrition::NutritionFacts;
pub use pantry::{PantryItem, PantryItemApi};
pub use people::{
    CommunicationRef, ContactMethod, OrganizationContext, OrganizationRecord, Person,
    PersonContext, ProviderConflict, ProviderConflictField, ProviderRef, Relationship,
};
pub use project::{
    Project, ProjectDashboardBucket, ProjectDashboardEntry, ProjectStats, ProjectStatus, next_task,
    project_dashboard,
};
pub use property::{PropertyDefinition, PropertyKind};
pub use query::{Filter, Group, GroupedTasks, Query, Sort, TaskGroup};
pub use reaction::Reaction as EntityReaction;
pub use recipe::{
    Recipe, RecipeApi, RecipeIngredientSpec, RecipeStepSpec, ScaleResult, scale_ingredients,
};
pub use recipe_ingredient::{RecipeIngredient, RecipeIngredientApi};
pub use recipe_step::{RecipeStep, RecipeStepApi};
pub use revenue::{
    Revenue, RevenueBucket, RevenueCreateRequest, RevenueFilter, RevenueReport, format_revenue_id,
};
pub use routine::{Routine, RoutineApi, RoutineTagList};
pub use routine_exercise::{RoutineExercise, RoutineExerciseApi};
pub use set_log::{SetLog, SetLogApi};
pub use shopping_list::{ShoppingList, ShoppingListApi, ShoppingListItem, ShoppingListItemApi};
pub use substitution::{Substitution, SubstitutionApi};
pub use task::{
    DependencyRelType, Priority, Reaction, RecurrenceAnchor, RelationType, Reminder,
    ReminderAnchor, Status, Task, TaskDependency, TaskRelation, TimeEntry, WikiLink,
};
pub use task_relation::TaskRelation as TaskRelationEntity;
pub use track::{Track, TrackApi, TrackStatus};
pub use views::{SavedView, ViewDisplay, ViewFilters};
pub use workout_session::{WorkoutSession, WorkoutSessionApi, WorkoutSessionStatus};

#[cfg(feature = "server")]
pub use integration::{
    Integration, ProjectTemplate, StatusDef, TaskTemplate, list_integrations, load_integration,
};
#[cfg(feature = "server")]
pub use provider::{
    ChannelConversation, ChannelMessage, ChannelSendMessageRequest, CommunicationChannelProvider,
    NextcloudSync,
};
#[cfg(feature = "server")]
pub use service::{
    ActivityService, ActivityServiceDispatcher, AddRoutineExerciseRequest, AddShoppingItemRequest,
    AddToPantryRequest, AddTrackRequest, AttachmentDownloadResponse, AttachmentService,
    AttachmentServiceDispatcher, AttachmentUploadRequest, AudioProductionService,
    AudioProductionServiceDispatcher, BarcodeLookupRequest, BodyMeasurementTrendRequest,
    BodyMeasurementTrendView, BusinessFinanceClientSummary, BusinessFinanceReport,
    CalDavCalendarInfo, CalDavDeleteObjectRequest, CalDavDiscovery, CalDavFreeBusyInterval,
    CalDavFreeBusyRequest, CalDavMultigetRequest, CalDavObject, CalDavPutObjectRequest,
    CalDavScheduleRequest, CalDavScheduleResponse, CalDavSyncCollectionRequest,
    CalDavSyncCollectionResponse, CalendarEventPatch, CalendarService, CalendarServiceDispatcher,
    CardDavAddressBookInfo, CardDavContact, CardDavDeleteObjectRequest, CardDavDiscovery,
    CardDavMultigetRequest, CardDavObject, CardDavPutObjectRequest, CardDavSyncCollectionRequest,
    CardDavSyncCollectionResponse, CompleteCookingSessionRequest, CompleteWorkoutSessionRequest,
    ConsumeFromPantryRequest, ConversationService, ConversationServiceDispatcher,
    CookbookWithRecipes, CookingService, CookingServiceClient, CookingServiceDispatcher,
    CookingSessionView, CreateExerciseRequest, CreateFoodProductRequest, CreateFoodRequest,
    CreateGlossaryTermRequest, CreateRecipeRequest, CreateRoutineRequest, EmailLinkRequest,
    EmailLinkResponse, EmailListRequest, EmailUnlinkRequest, ExercisePatch, ExpenseService,
    ExpenseServiceDispatcher, FitnessService, FitnessServiceClient, FitnessServiceDispatcher,
    FoodPatch, FoodProductPatch, FoodService, FoodServiceClient, FoodServiceDispatcher,
    GenerateShoppingListFromMissingRequest, GenerateShoppingListRequest, GlossaryService,
    GlossaryServiceClient, GlossaryServiceDispatcher, GlossaryTermPatch, HealthCheck,
    InboxCaptureRequest, InboxItem, InboxPromoteRequest, InboxService, InboxServiceDispatcher,
    InvoiceAgingBucket, InvoiceCreateRequest, InvoicePaymentRequest, InvoiceService,
    InvoiceServiceDispatcher, ListBodyMeasurementsRequest, LogSetRequest, MailCreateMailboxRequest,
    MailCreateTagRequest, MailDeleteTagRequest, MailListMessagesRequest, MailMessageTagRequest,
    MailMoveMessageRequest, MailService, MailServiceDispatcher, MarkIngredientGatheredRequest,
    MarkMealPlanCookedRequest, MaterializeRequest, MaterializeResult, MealPlanRangeRequest,
    MetricTrend, NavigateStepRequest, NextcloudCapability, OperatingAreaStatus, OperatingGoal,
    OperatingModelReport, OperatingRoutine, OperatingService, OperatingServiceDispatcher,
    PantryItemPatch, PantryListRequest, PantryService, PantryServiceClient,
    PantryServiceDispatcher, PeopleService, PeopleServiceDispatcher, ProjectFileSummary,
    ProjectKnowledgeContext, ProjectPatch, ProjectService, ProjectServiceDispatcher,
    ProjectTypeService, ProjectTypeServiceDispatcher, ProjectTypeSpec, ProjectTypeView,
    PropertyDefinitionView, PropertyService, PropertyServiceDispatcher, ProviderSyncState,
    RecipeMatchView, RecipePatch, RecipeWithDetails, RecordBodyMeasurementRequest, RemoteDeckBoard,
    RemoteDeckStack, ResolveInTextRequest, ResolveInTextView, ReviewReport,
    RoutineWithExercisesView, ScaledRecipeView, SetMealPlanEntryRequest, ShoppingListWithItems,
    StartCookingSessionRequest, StartWorkoutSessionRequest, StepTimerActionRequest,
    SubmitMixRequest, SyncPlan, SyncPlanItem, SyncStats, SystemCapabilities, SystemHealth,
    SystemService, SystemServiceDispatcher, TaskService, TaskServiceDispatcher, TemplateService,
    TemplateServiceDispatcher, TemplateView, TimeEntryContext, TimeEntryFilter, TimeEntryPatch,
    TimeLogRequest, TimeService, TimeServiceDispatcher, TimeStartRequest, TimedTaskEntry,
    TrackPatch, UpdateBodyMeasurementRequest, UpdateSetRequest, VaultCapability, VaultError,
    WorkoutSessionView, activity_service_service_descriptor, attachment_service_service_descriptor,
    audio_production_service_service_descriptor, calendar_service_service_descriptor,
    conversation_service_service_descriptor, cooking_service_service_descriptor,
    expense_service_service_descriptor, fitness_service_service_descriptor,
    food_service_service_descriptor, glossary_service_service_descriptor,
    inbox_service_service_descriptor, invoice_service_service_descriptor,
    mail_service_service_descriptor, operating_service_service_descriptor,
    pantry_service_service_descriptor, people_service_service_descriptor,
    project_service_service_descriptor, project_type_service_service_descriptor,
    property_service_service_descriptor, system_service_service_descriptor,
    task_service_service_descriptor, template_service_service_descriptor,
    time_service_service_descriptor,
};
#[cfg(feature = "server")]
pub use service_impl::{
    ActivityServiceImpl, AttachmentServiceImpl, AudioProductionServiceImpl, CalendarServiceImpl,
    ConversationServiceImpl, ExpenseServiceImpl, FitnessServiceImpl, FoodServiceImpl,
    GlossaryServiceImpl, InboxServiceImpl, InvoiceServiceImpl, MailServiceImpl,
    OperatingServiceImpl, PantryServiceImpl, PeopleServiceImpl, ProjectServiceImpl,
    ProjectTypeServiceImpl, PropertyServiceImpl, TaskServiceImpl, TemplateServiceImpl,
    TimeServiceImpl,
};

#[cfg(feature = "caldav")]
#[cfg(feature = "realtime")]
pub use caldav::{
    CalDavClient, CalDavConfig, ics_to_task, task_to_ics, task_to_vtodo, vtodo_to_task,
};
