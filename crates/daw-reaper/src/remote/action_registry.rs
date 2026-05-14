//! Remote action registry handle: [`RemoteActionRegistry`].

use daw_proto::DawResult;
use daw_proto::{ActionRegistry as ActionRegistryTrait, Daw as _};

use super::{ReaperRemote, dispatch, main_thread};

pub struct RemoteActionRegistry<'a> {
    remote: &'a ReaperRemote,
}

impl<'a> RemoteActionRegistry<'a> {
    pub(crate) fn new(remote: &'a ReaperRemote) -> Self {
        Self { remote }
    }
}

impl<'a> ActionRegistryTrait for RemoteActionRegistry<'a> {
    fn register(&self, cmd_name: &str, description: &str) -> DawResult<u32> {
        let cmd_name = cmd_name.to_string();
        let description = description.to_string();
        dispatch(self.remote, move || {
            let mt = main_thread()?;
            mt.action_registry().register(&cmd_name, &description)
        })
    }

    fn register_in_menu(&self, cmd_name: &str, description: &str) -> DawResult<u32> {
        let cmd_name = cmd_name.to_string();
        let description = description.to_string();
        dispatch(self.remote, move || {
            let mt = main_thread()?;
            mt.action_registry()
                .register_in_menu(&cmd_name, &description)
        })
    }

    fn register_toggle(&self, cmd_name: &str, description: &str) -> DawResult<u32> {
        let cmd_name = cmd_name.to_string();
        let description = description.to_string();
        dispatch(self.remote, move || {
            let mt = main_thread()?;
            mt.action_registry()
                .register_toggle(&cmd_name, &description)
        })
    }

    fn register_toggle_in_menu(&self, cmd_name: &str, description: &str) -> DawResult<u32> {
        let cmd_name = cmd_name.to_string();
        let description = description.to_string();
        dispatch(self.remote, move || {
            let mt = main_thread()?;
            mt.action_registry()
                .register_toggle_in_menu(&cmd_name, &description)
        })
    }

    fn unregister(&self, cmd_name: &str) -> DawResult<()> {
        let cmd_name = cmd_name.to_string();
        dispatch(self.remote, move || {
            let mt = main_thread()?;
            mt.action_registry().unregister(&cmd_name)
        })
    }
}
