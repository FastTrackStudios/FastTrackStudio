pub mod activity;
pub mod agent;
pub mod asset;
pub mod attachment;
pub mod calendar_event;
pub mod capture;
pub mod client;
pub mod comment;
pub mod cookbook;
pub mod cookbook_recipe;
pub mod cycle;
pub mod email;
pub mod expense;
pub mod food;
pub mod food_product;
pub mod invoice;
pub mod location;
pub mod meal_plan;
pub mod module;
pub mod notification;
pub mod nutrition;
pub mod people;
pub mod project;
pub mod property;
pub mod query;
pub mod reaction;
pub mod recipe;
pub mod recipe_ingredient;
pub mod recipe_step;
pub mod revenue;
pub mod rrule;
pub mod shopping_list;
pub mod task;
pub mod task_relation;
pub mod team;
pub mod track;
pub mod views;
pub mod workflows;

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
pub use calendar_event::{CalendarEvent, CalendarEventStatus};
pub use capture::{CaptureInput, parse_capture};
pub use client::{Client, resolve_rate};
pub use comment::Comment;
pub use cookbook::{Cookbook, CookbookApi};
pub use cookbook_recipe::{CookbookRecipe, CookbookRecipeApi};
pub use cycle::{Cycle, CycleStatus};
pub use email::EmailRef;
pub use expense::{
    Expense, ExpenseBucket, ExpenseCreateRequest, ExpensePatch, ExpenseReport, ExpenseStatus,
    format_expense_id,
};
pub use food::{Food, FoodAliasList, FoodApi};
pub use food_product::{FoodProduct, FoodProductApi};
pub use invoice::{Invoice, InvoiceLine, InvoiceStatus, Payment, format_invoice_id};
pub use location::{Location, Space, VenueDefault, render_location_body};
pub use meal_plan::{MealPlanEntry, MealPlanEntryApi, MealType};
pub use module::{Module, ModuleStatus};
pub use notification::Notification;
pub use nutrition::NutritionFacts;
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
pub use recipe::{Recipe, RecipeApi, RecipeIngredientSpec, RecipeStepSpec};
pub use recipe_ingredient::{RecipeIngredient, RecipeIngredientApi};
pub use recipe_step::{RecipeStep, RecipeStepApi};
pub use revenue::{
    Revenue, RevenueBucket, RevenueCreateRequest, RevenueFilter, RevenueReport, format_revenue_id,
};
pub use shopping_list::{ShoppingList, ShoppingListApi, ShoppingListItem, ShoppingListItemApi};
pub use task::{
    DependencyRelType, Priority, Reaction, RecurrenceAnchor, RelationType, Reminder,
    ReminderAnchor, Status, Task, TaskDependency, TaskRelation, TimeEntry, WikiLink,
};
pub use task_relation::TaskRelation as TaskRelationEntity;
pub use track::{Track, TrackApi, TrackStatus};
pub use views::{SavedView, ViewDisplay, ViewFilters};

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
    ActivityService, ActivityServiceDispatcher, AddShoppingItemRequest, AddTrackRequest,
    AttachmentDownloadResponse, AttachmentService, AttachmentServiceDispatcher,
    AttachmentUploadRequest, AudioProductionService, AudioProductionServiceDispatcher,
    BarcodeLookupRequest, BusinessFinanceClientSummary, BusinessFinanceReport, CalDavCalendarInfo,
    CalDavDeleteObjectRequest, CalDavDiscovery, CalDavFreeBusyInterval, CalDavFreeBusyRequest,
    CalDavMultigetRequest, CalDavObject, CalDavPutObjectRequest, CalDavScheduleRequest,
    CalDavScheduleResponse, CalDavSyncCollectionRequest, CalDavSyncCollectionResponse,
    CalendarEventPatch, CalendarService, CalendarServiceDispatcher, CardDavAddressBookInfo,
    CardDavContact, CardDavDeleteObjectRequest, CardDavDiscovery, CardDavMultigetRequest,
    CardDavObject, CardDavPutObjectRequest, CardDavSyncCollectionRequest,
    CardDavSyncCollectionResponse, ConversationService, ConversationServiceDispatcher,
    CookbookWithRecipes, CookingService, CookingServiceClient, CookingServiceDispatcher,
    CreateFoodProductRequest, CreateFoodRequest, CreateRecipeRequest, EmailLinkRequest,
    EmailLinkResponse, EmailListRequest, EmailUnlinkRequest, ExpenseService,
    ExpenseServiceDispatcher, FoodPatch, FoodProductPatch, FoodService, FoodServiceClient,
    FoodServiceDispatcher, GenerateShoppingListRequest, HealthCheck, InboxCaptureRequest,
    InboxItem, InboxPromoteRequest, InboxService, InboxServiceDispatcher, InvoiceAgingBucket,
    InvoiceCreateRequest, InvoicePaymentRequest, InvoiceService, InvoiceServiceDispatcher,
    MailCreateMailboxRequest, MailCreateTagRequest, MailDeleteTagRequest, MailListMessagesRequest,
    MailMessageTagRequest, MailMoveMessageRequest, MailService, MailServiceDispatcher,
    MaterializeRequest, MaterializeResult, MealPlanRangeRequest, NextcloudCapability,
    OperatingAreaStatus, OperatingGoal, OperatingModelReport, OperatingRoutine, OperatingService,
    OperatingServiceDispatcher, PeopleService, PeopleServiceDispatcher, ProjectFileSummary,
    ProjectKnowledgeContext, ProjectPatch, ProjectService, ProjectServiceDispatcher,
    ProjectTypeService, ProjectTypeServiceDispatcher, ProjectTypeSpec, ProjectTypeView,
    PropertyDefinitionView, PropertyService, PropertyServiceDispatcher, ProviderSyncState,
    RecipePatch, RecipeWithDetails, RemoteDeckBoard, RemoteDeckStack, ReviewReport,
    SetMealPlanEntryRequest, ShoppingListWithItems, SubmitMixRequest, SyncPlan, SyncPlanItem,
    SyncStats, SystemCapabilities, SystemHealth, SystemService, SystemServiceDispatcher,
    TaskService, TaskServiceDispatcher, TemplateService, TemplateServiceDispatcher, TemplateView,
    TimeEntryContext, TimeEntryFilter, TimeEntryPatch, TimeLogRequest, TimeService,
    TimeServiceDispatcher, TimeStartRequest, TimedTaskEntry, TrackPatch, VaultCapability,
    VaultError, activity_service_service_descriptor, attachment_service_service_descriptor,
    audio_production_service_service_descriptor, calendar_service_service_descriptor,
    conversation_service_service_descriptor, cooking_service_service_descriptor,
    expense_service_service_descriptor, food_service_service_descriptor,
    inbox_service_service_descriptor, invoice_service_service_descriptor,
    mail_service_service_descriptor, operating_service_service_descriptor,
    people_service_service_descriptor, project_service_service_descriptor,
    project_type_service_service_descriptor, property_service_service_descriptor,
    system_service_service_descriptor, task_service_service_descriptor,
    template_service_service_descriptor, time_service_service_descriptor,
};
#[cfg(feature = "server")]
pub use service_impl::{
    ActivityServiceImpl, AttachmentServiceImpl, AudioProductionServiceImpl, CalendarServiceImpl,
    ConversationServiceImpl, ExpenseServiceImpl, FoodServiceImpl, InboxServiceImpl,
    InvoiceServiceImpl, MailServiceImpl, OperatingServiceImpl, PeopleServiceImpl,
    ProjectServiceImpl, ProjectTypeServiceImpl, PropertyServiceImpl, TaskServiceImpl,
    TemplateServiceImpl, TimeServiceImpl,
};

#[cfg(feature = "caldav")]
#[cfg(feature = "realtime")]
pub use caldav::{
    CalDavClient, CalDavConfig, ics_to_task, task_to_ics, task_to_vtodo, vtodo_to_task,
};
