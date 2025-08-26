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
    plug :fetch_current_scope_for_customer
  end

  pipeline :api do
    plug :accepts, ["json"]
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
    pipe_through [:browser, :require_authenticated_customer]

    live_session :require_authenticated_customer,
      on_mount: [{DmarcWeb.CustomerAuth, :require_authenticated}] do
      live "/customers/settings", CustomerLive.Settings, :edit
      live "/customers/settings/confirm-email/:token", CustomerLive.Settings, :confirm_email

      live "/tenants", TenantLive.Index, :index
      live "/tenants/new", TenantLive.Form, :new
      live "/tenants/:id", TenantLive.Show, :show
      live "/tenants/:id/edit", TenantLive.Form, :edit

      live "/recipients", RecipientLive.Index, :index
      live "/recipients/new", RecipientLive.Form, :new
      live "/recipients/:id", RecipientLive.Show, :show
      live "/recipients/:id/edit", RecipientLive.Form, :edit

      live "/query/count/from/:start/until/:end", Queries.QueryLive
      live "/query/count", Queries.QueryLive
      live "/query/ips", Queries.QueryIps
      live "/query/days", Queries.QueryDays
    end

    post "/customers/update-password", CustomerSessionController, :update_password
    get "/", PageController, :home
  end

  scope "/", DmarcWeb do
    pipe_through [:browser]

    live_session :current_customer,
      on_mount: [{DmarcWeb.CustomerAuth, :mount_current_scope}] do
      live "/customers/register", CustomerLive.Registration, :new
      live "/customers/log-in", CustomerLive.Login, :new
      live "/customers/log-in/:token", CustomerLive.Confirmation, :new
    end

    post "/customers/log-in", CustomerSessionController, :create
    delete "/customers/log-out", CustomerSessionController, :delete
  end
end
