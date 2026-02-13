//! Auth UI — login/signup screen

use super::control::AuthControl;
use crate::example::ui_state::AppUiContext;
use dioxus::prelude::*;

#[component]
pub fn AuthScreen() -> Element {
    let app = use_context::<AppUiContext>();
    let mut is_signup = use_signal(|| false);
    let mut email = use_signal(|| "demo@example.com".to_string());
    let mut password = use_signal(|| "demo123".to_string());
    let mut name = use_signal(String::new);
    let mut loading = use_signal(|| false);
    let login_error = app.login_error;
    let error = login_error.read();

    let mut submit = move || {
        if *loading.read() {
            return;
        }
        loading.set(true);

        let mut login_error = login_error;
        login_error.set(None);

        let email_val = email.read().clone();
        let password_val = password.read().clone();
        let name_val = name.read().clone();
        let signup = *is_signup.read();
        let svc = app.auth_service.clone();
        let mut auth_state = app.auth_state;

        let anon = AuthControl::new(svc);

        spawn(async move {
            let result = if signup {
                anon.sign_up(&email_val, &password_val, &name_val).await
            } else {
                anon.sign_in(&email_val, &password_val).await
            };

            match result {
                Ok(authed) => {
                    auth_state.set(Some(authed));
                }
                Err((_anon_back, msg)) => {
                    let mut login_error = login_error;
                    login_error.set(Some(msg));
                }
            }
            loading.set(false);
        });
    };

    rsx! {
        div { class: "flex items-center justify-center min-h-screen",
            div { class: "w-full max-w-sm mx-auto",
                div { class: "text-center mb-8",
                    h1 { class: "text-3xl font-bold text-zinc-100 mb-2", "Architecture Playground" }
                    p { class: "text-zinc-500 text-sm",
                        "better-auth -> Service -> Control (typestate) -> UI"
                    }
                }

                div { class: "bg-zinc-900 border border-zinc-800 rounded-lg p-6 shadow-xl",
                    div { class: "flex mb-6 bg-zinc-800 rounded-md p-1",
                        button {
                            class: if !*is_signup.read() {
                                "flex-1 py-1.5 text-sm font-medium rounded bg-zinc-700 text-zinc-100"
                            } else {
                                "flex-1 py-1.5 text-sm font-medium rounded text-zinc-400 hover:text-zinc-300"
                            },
                            onclick: move |_| is_signup.set(false),
                            "Sign In"
                        }
                        button {
                            class: if *is_signup.read() {
                                "flex-1 py-1.5 text-sm font-medium rounded bg-zinc-700 text-zinc-100"
                            } else {
                                "flex-1 py-1.5 text-sm font-medium rounded text-zinc-400 hover:text-zinc-300"
                            },
                            onclick: move |_| is_signup.set(true),
                            "Sign Up"
                        }
                    }

                    div {
                        if *is_signup.read() {
                            div { class: "mb-4",
                                label { class: "block text-sm font-medium text-zinc-400 mb-1", "Name" }
                                input {
                                    class: "w-full px-3 py-2 bg-zinc-800 border border-zinc-700 rounded-md text-zinc-100 placeholder-zinc-500 focus:outline-none focus:ring-2 focus:ring-blue-500 focus:border-transparent",
                                    r#type: "text",
                                    placeholder: "Your name",
                                    value: "{name}",
                                    oninput: move |e| name.set(e.value()),
                                }
                            }
                        }

                        div { class: "mb-4",
                            label { class: "block text-sm font-medium text-zinc-400 mb-1", "Email" }
                            input {
                                class: "w-full px-3 py-2 bg-zinc-800 border border-zinc-700 rounded-md text-zinc-100 placeholder-zinc-500 focus:outline-none focus:ring-2 focus:ring-blue-500 focus:border-transparent",
                                r#type: "email",
                                placeholder: "you@example.com",
                                value: "{email}",
                                oninput: move |e| email.set(e.value()),
                            }
                        }

                        div { class: "mb-4",
                            label { class: "block text-sm font-medium text-zinc-400 mb-1", "Password" }
                            input {
                                class: "w-full px-3 py-2 bg-zinc-800 border border-zinc-700 rounded-md text-zinc-100 placeholder-zinc-500 focus:outline-none focus:ring-2 focus:ring-blue-500 focus:border-transparent",
                                r#type: "password",
                                placeholder: "Min 6 characters",
                                value: "{password}",
                                oninput: move |e| password.set(e.value()),
                            }
                        }

                        if let Some(err) = error.as_ref() {
                            div { class: "mb-4 px-3 py-2 bg-red-900/30 border border-red-800 rounded-md text-red-400 text-sm",
                                "{err}"
                            }
                        }

                        button {
                            class: "w-full py-2 px-4 bg-blue-600 hover:bg-blue-500 disabled:bg-zinc-700 disabled:text-zinc-500 text-white font-medium rounded-md transition-colors",
                            onclick: move |_| submit(),
                            disabled: *loading.read(),
                            if *loading.read() { "Working..." }
                            else if *is_signup.read() { "Create Account" }
                            else { "Sign In" }
                        }
                    }
                }

                p { class: "mt-4 text-xs text-zinc-600 text-center",
                    "Demo credentials: demo@example.com / demo123"
                }

                div { class: "mt-4 p-4 bg-zinc-900/50 border border-zinc-800/50 rounded-lg",
                    p { class: "text-xs text-zinc-500 font-mono leading-relaxed",
                        "AuthControl<Anonymous> -> sign_in() -> AuthControl<Authenticated>"
                    }
                    p { class: "text-xs text-zinc-600 mt-1",
                        "Compile-time enforcement: items unavailable before auth."
                    }
                }
            }
        }
    }
}

#[component]
pub fn SignOutButton() -> Element {
    let app = use_context::<AppUiContext>();

    let on_signout = move |_| {
        let mut auth_state = app.auth_state;
        let ctl = auth_state.write().take();
        if let Some(ctl) = ctl {
            spawn(async move {
                ctl.sign_out().await;
            });
        }
    };

    rsx! {
        button {
            class: "px-3 py-1.5 text-sm text-zinc-400 hover:text-zinc-200 border border-zinc-700 hover:border-zinc-600 rounded-md transition-colors",
            onclick: on_signout,
            "Sign Out"
        }
    }
}
