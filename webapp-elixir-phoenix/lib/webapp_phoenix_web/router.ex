defmodule WebappPhoenixWeb.Router do
  use WebappPhoenixWeb, :router

  pipeline :browser do
    plug :accepts, ["html"]
    plug :fetch_session
    plug :fetch_live_flash
    plug :put_root_layout, html: {WebappPhoenixWeb.Layouts, :root}
    plug :protect_from_forgery
    plug :put_secure_browser_headers
  end

  pipeline :api do
    plug :accepts, ["json"]
  end

  pipeline :authenticated do
    plug WebappPhoenixWeb.Plugs.Auth, "remote-user"
  end

  scope "/", WebappPhoenixWeb do
    pipe_through [:browser, :authenticated]

    get "/", PageController, :home
    # get "/query/count/from/:start/until/:end", QueryController, :count
    live "/query/count/from/:start/until/:end", QueryLive
    live "/query/count", QueryLive
  end

  # Other scopes may use custom stacks.
  # scope "/api", WebappPhoenixWeb do
  #   pipe_through :api
  # end

  # Enable LiveDashboard and Swoosh mailbox preview in development
  if Application.compile_env(:webapp_phoenix, :dev_routes) do
    # If you want to use the LiveDashboard in production, you should put
    # it behind authentication and allow only admins to access it.
    # If your application does not have an admins-only section yet,
    # you can use Plug.BasicAuth to set up some basic authentication
    # as long as you are also using SSL (which you should anyway).
    import Phoenix.LiveDashboard.Router

    scope "/dev" do
      pipe_through :browser

      live_dashboard "/dashboard", metrics: WebappPhoenixWeb.Telemetry
      forward "/mailbox", Plug.Swoosh.MailboxPreview
    end
  end
end
