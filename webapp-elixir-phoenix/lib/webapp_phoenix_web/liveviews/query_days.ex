defmodule WebappPhoenixWeb.QueryDays do
  @moduledoc """
  This module contains pages rendered by PageController.

  See the `page_html` directory for all templates available.
  """
  # use WebappPhoenixWeb, :html
  use WebappPhoenixWeb, :live_view

  require Logger
  import SweetXml

  # embed_templates "query_html/*"

  def date_input(assigns) do
    ~H"""
    <label class="label">{@field.name}</label>
    <input type="date" class="input" name={@field.name} id={@field.id} value={@field.value} />
    """
  end

  defp query(tenant, startdate, enddate) do
    startts = with {:ok, startdt} <- DateTime.new(startdate, ~T[00:00:00], "Etc/UTC") do
      DateTime.to_unix(startdt)
    end
    endts = with {:ok, enddt} <- DateTime.new(enddate, ~T[00:00:00], "Etc/UTC") do
      DateTime.to_unix(enddt)
    end

    {:ok, result_stream} = Db.ExistDb.query(Db.ExistDb,
      tenant,
      "query_days",
      %{wantedBegin: startts, wantedEnd: endts}
    )

    result_stream
      |> Stream.map(fn {:item, xml} ->
        xml |> xpath(
          ~x"/item",
          dmarc: ~x"./dmarc/text()"s,
          date: ~x"./date/text()"s,
          count: ~x"./count/text()"f,
          reportcount: ~x"./reportcount/text()"i,
          ipcount: ~x"./ipcount/text()"i,
          reports: [~x"./reports/report"l, id: ~x"./id/text()"s, email: ~x"./email/text()"]
        )
      end)
      |> Enum.to_list()
  end

  defp param_to_date(param) when is_integer(param) do
    with {:ok, d} <- DateTime.from_unix(param) do
      {:ok, DateTime.to_date(d)}
    end
  end

  defp param_to_date(param) when is_binary(param) do
    with {i, _} <- Integer.parse(param) do
      param_to_date(i)
    end
  end

  defp param_to_date(param) when is_nil(param) do
    :error
  end

  def mount(params, session, socket) do
    Logger.debug([module: __MODULE__, message: "MOUNT"])
    # IO.inspect(params)
    # IO.inspect(session)

    tenant = Map.fetch!(session, "authuser")
    startdate = case param_to_date(params["start"]) do
      {:ok, date} -> date
      _ -> DateTime.now!("Etc/UTC")
        |> DateTime.add(-30, :day)
        |> DateTime.to_date()
    end
    enddate = case param_to_date(params["end"]) do
      {:ok, date} -> date
      _ -> DateTime.now!("Etc/UTC")
        |> DateTime.to_date()
    end
    # results = query(tenant, ~D[2024-06-01], ~D[2025-03-31])
    results = query(tenant, startdate, enddate)

    {:ok, assign(socket,
      tenant: tenant,
      form: to_form(%{"start" => startdate, "end" => enddate}),
      startdate: startdate,
      enddate: enddate,
      results: results,
      query_tref: nil
    )}
  end

  def handle_event("change_date", params, socket) do
    Logger.debug([module: __MODULE__, message: "HANDLE_EVENT CHANGE_DATE"])
    # IO.inspect(params)
    # IO.inspect(socket)
    {:ok, startdate} = Date.from_iso8601(params["start"])
    {:ok, enddate} = Date.from_iso8601(params["end"])
    # tenant = socket.assigns[:tenant]
    # results = query(tenant, startdate, enddate)

    # start_async(socket, :query, fn -> :foobar end)
    prev_tref = socket.assigns[:query_tref]
    if prev_tref != nil do
      :timer.cancel(prev_tref)
    end
    pid = self()
    {:ok, tref} = :timer.apply_after(
      :timer.seconds(2),
      fn -> GenServer.cast(pid, :query) end
    )

    {:noreply, assign(socket, startdate: startdate, enddate: enddate, query_tref: tref)}
  end

  def handle_cast(:query, socket) do
    Logger.debug([module: __MODULE__, message: "HANDLE_CAST QUERY"])
    # IO.inspect(params)
    # IO.inspect(socket)

    startdate = socket.assigns[:startdate]
    enddate = socket.assigns[:enddate]
    tenant = socket.assigns[:tenant]
    results = query(tenant, startdate, enddate)

    {:noreply, assign(socket, results: results, query_tref: nil)}
  end
end

