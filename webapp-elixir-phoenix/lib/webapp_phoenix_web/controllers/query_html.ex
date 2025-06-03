defmodule WebappPhoenixWeb.QueryHTML do
  @moduledoc """
  This module contains pages rendered by PageController.

  See the `page_html` directory for all templates available.
  """
  use WebappPhoenixWeb, :html

  embed_templates "query_html/*"
end

