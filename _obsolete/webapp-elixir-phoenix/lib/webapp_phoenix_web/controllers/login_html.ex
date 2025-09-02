defmodule WebappPhoenixWeb.LoginHTML do
  @moduledoc """
  This module contains pages rendered by LoginController.

  See the `login_html` directory for all templates available.
  """
  use WebappPhoenixWeb, :html

  embed_templates "login_html/*"
end
