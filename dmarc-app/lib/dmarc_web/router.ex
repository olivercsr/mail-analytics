defmodule DmarcWeb.Router do
  use DmarcWeb, :router

  pipeline :browser do
    plug :accepts, ["html"]
    plug :fetch_session
    plug :fetch_live_flash
    plug :put_root_layout, html: {DmarcWeb.Layouts, :root}
    plug :protect_from_forgery
    plug :put_secure_browser_headers
  end

  pipeline :api do
    plug :accepts, ["json"]
  end

  pipeline :authenticated do
    plug DmarcWeb.Plugs.Auth, "remote-user"
  end

  pipeline :authenticated_jwt do
    plug DmarcWeb.Plugs.AuthJwt
  end

  scope "/login", DmarcWeb do
    pipe_through [:browser]

    get "/", LoginController, :index
    get "/:provider", AuthController, :index
    get "/:provider/callback", AuthController, :callback
  end

  scope "/", DmarcWeb do
    pipe_through [:browser, :authenticated_jwt]

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
end
