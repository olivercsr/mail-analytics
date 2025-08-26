defmodule DmarcWeb.Router do
  use DmarcWeb, :router

  import DmarcWeb.CustomerAuth

  pipeline :browser do
    plug :accepts, ["html"]
    plug :fetch_session
    plug :fetch_live_flash
    plug :put_root_layout, html: {DmarcWeb.Layouts, :root}
    plug :protect_from_forgery
    plug :put_secure_browser_headers
    plug :fetch_current_customer
  end

  pipeline :api do
    plug :accepts, ["json"]
  end

  # pipeline :authenticated do
  #   plug DmarcWeb.Plugs.Auth, "remote-user"
  # end
  #
  # pipeline :authenticated_jwt do
  #   plug DmarcWeb.Plugs.AuthJwt
  # end
  #
  #scope "/login", DmarcWeb do
  #  pipe_through [:browser]

  #  get "/", LoginController, :index
  #  get "/:provider", AuthController, :index
  #  get "/:provider/callback", AuthController, :callback
  #end

  scope "/", DmarcWeb do
    # pipe_through [:browser, :authenticated_jwt]
    pipe_through [:browser]

    get "/", PageController, :home
    # get "/query/count/from/:start/until/:end", QueryController, :count
    live "/query/count/from/:start/until/:end", QueryLive
    live "/query/count", QueryLive
    live "/query/ips", QueryIps
    live "/query/days", QueryDays
  end

  # Other scopes may use custom stacks.
  # scope "/api", DmarcWeb do
  #   pipe_through :api
  # end

  # Enable LiveDashboard and Swoosh mailbox preview in development
  if Application.compile_env(:dmarc, :dev_routes) do
    # If you want to use the LiveDashboard in production, you should put
    # it behind authentication and allow only admins to access it.
    # If your application does not have an admins-only section yet,
    # you can use Plug.BasicAuth to set up some basic authentication
    # as long as you are also using SSL (which you should anyway).
    import Phoenix.LiveDashboard.Router

    scope "/dev" do
      pipe_through :browser

      live_dashboard "/dashboard", metrics: DmarcWeb.Telemetry
      forward "/mailbox", Plug.Swoosh.MailboxPreview
    end
  end

  ## Authentication routes

  scope "/", DmarcWeb do
    pipe_through [:browser, :redirect_if_customer_is_authenticated]

    live_session :redirect_if_customer_is_authenticated,
      on_mount: [{DmarcWeb.CustomerAuth, :redirect_if_customer_is_authenticated}] do
      live "/customers/register", CustomerRegistrationLive, :new
      live "/customers/log_in", CustomerLoginLive, :new
      live "/customers/reset_password", CustomerForgotPasswordLive, :new
      live "/customers/reset_password/:token", CustomerResetPasswordLive, :edit
    end

    post "/customers/log_in", CustomerSessionController, :create
  end

  scope "/", DmarcWeb do
    pipe_through [:browser, :require_authenticated_customer]

    live_session :require_authenticated_customer,
      on_mount: [{DmarcWeb.CustomerAuth, :ensure_authenticated}] do
      live "/customers/settings", CustomerSettingsLive, :edit
      live "/customers/settings/confirm_email/:token", CustomerSettingsLive, :confirm_email
    end
  end

  scope "/", DmarcWeb do
    pipe_through [:browser]

    delete "/customers/log_out", CustomerSessionController, :delete

    live_session :current_customer,
      on_mount: [{DmarcWeb.CustomerAuth, :mount_current_customer}] do
      live "/customers/confirm/:token", CustomerConfirmationLive, :edit
      live "/customers/confirm", CustomerConfirmationInstructionsLive, :new
    end
  end
end
