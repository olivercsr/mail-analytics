defmodule WebappPhoenixWeb.QueryLive do
  @moduledoc """
  This module contains pages rendered by PageController.

  See the `page_html` directory for all templates available.
  """
  # use WebappPhoenixWeb, :html
  use WebappPhoenixWeb, :live_view

  import SweetXml

  # embed_templates "query_html/*"

  def mount(%{"start" => startts, "end" => endts} = params, session, socket) do
    IO.puts("MOUNT")
    IO.inspect(params)
    IO.inspect(session)

    # TODO: get tenant from session
    # TODO: add ui controls to adjust start & end, which will then in turn trigger re-query

    {startts_int, ""} = Integer.parse(startts)
    {endts_int, ""} = Integer.parse(endts)
    # existdb_config = Application.get_env(:webapp_phoenix, Db.ExistDb)
    # [tenant] = Plug.Conn.get_req_header(conn, "remote-user") # TODO: lookup mail addresses
    #   |> Enum.map(&String.trim/1)
    # tenant = conn.assigns.authuser
    tenant = "dmarc@csr-informatik.de"

    {:ok, result_stream} = Db.ExistDb.query(Db.ExistDb,
      tenant,
      "query_count",
      %{wantedBegin: startts_int, wantedEnd: endts_int}
    )
    results = result_stream
      |> Stream.map(fn {:item, xml} ->
        xml |> xpath(
          ~x"/item",
          begin: ~x"./begin/text()"s,
          end: ~x"./end/text()"s,
          reportscount: ~x"./reportscount/text()"i,
          sum: ~x"./proportionalrowcountsum/text()"i,
          spf: ~x"./spf/text()"s,
          dkim: ~x"./dkim/text()"s
        )
      end)
      |> Enum.to_list()

    {:ok, assign(socket, tenant: tenant, startts: startts, endts: endts, results: results, myrnd: 0)}
  end

  def handle_event("newrnd", params, socket) do
    IO.puts("HANDLE_EVENT")
    IO.inspect(params)
    {:noreply, update(socket, :myrnd, fn _ -> IO.puts("update"); :rand.uniform(1000) end)}
  end
end

