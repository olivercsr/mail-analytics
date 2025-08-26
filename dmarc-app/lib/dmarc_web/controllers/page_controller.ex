defmodule DmarcWeb.PageController do
  use DmarcWeb, :controller

  def home(conn, _params) do
    render(conn, :home)
  end
end
