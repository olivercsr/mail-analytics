defmodule Db.ExistDb do
  use GenServer

  require Logger
  require Finch
  require Req
  import SweetXml

  defmodule Config do
    defstruct [:base_url, :user, :password]
  end

  # Client

  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts, name: opts[:name])
  end

  def query(pid, tenant, query_name, variables) do
    GenServer.call(pid, {:query, tenant, query_name, variables})
  end

  def store(pid, tenant, filename, data_stream) do
    GenServer.call(pid, {:store, tenant, filename, data_stream})
  end

  # Server

  @impl true
  def init(opts) do
    initial_state = %{config: opts[:config]}

    Logger.info([module: __MODULE__, message: "ExistDb init", initial_state: initial_state])

    {:ok, initial_state}
  end

  defp type_of_value(value) do
    # IO.inspect(value)
    cond do
      is_integer(value) -> "integer"
      is_binary(value) -> "string"
    end
  end

  @impl true
  def handle_call({:query, tenant, query_name, variables}, _from, %{:config => config} = state)
    when is_binary(tenant) and tenant != "" and tenant != nil do
    query = EEx.eval_file("priv/xqueries/#{query_name}.eex", [query_name: query_name])
    variables = Enum.map(
      Map.put(variables, :tenant, tenant),
      fn {k, v} -> [key: k, type: type_of_value(v), value: v] end
    )
    xml_query = EEx.eval_file("priv/xqueries/container.eex", [query: query, variables: variables])
    # IO.puts(xml_query)
    # [
    #   [key: "wantedBegin", type: "integer", value: 1715689600],
    #   [key: "wantedEnd", type: "integer", value: 1742974400]
    # ]

    # Logger.info(EEx.eval_string("query_name: <%= query_name %>", [query_name: query_name]))
    # Logger.info("query: #{xml_query}")

    req = Req.new(
      url: "#{config.base_url}/#{tenant}",
      headers: %{"content-type" => ["application/xml"]},
      body: xml_query
    )
    # IO.inspect(req)

    case Req.post(req, finch: DmarcFinchPool) do
      {:ok, result} when result.status >= 200 and result.status < 300 -> 
        data = result.body |> stream_tags(:item)
        {:reply, {:ok, data}, state}
      {:ok, result} ->
        {:reply, {:error, "unsuccessful http response code: #{result.status} - #{result.body}"}, state}
      {:error, exception} ->
        {:reply, {:error, exception}, state}
    end
  end

  def handle_call({:store, tenant, filename, data_stream}, _from, %{:config => config} = state)
    when is_binary(tenant) and tenant != "" and tenant != nil do
    # TODO: make this entire request-making logic generic:
    req = Req.new(
      # TODO: url-encode filename?
      url: "#{config.base_url}/#{tenant}/#{filename}",
      auth: {:basic, "#{config.user}:#{config.password}"},
      headers: %{"content-type" => ["application/xml"]},
      body: data_stream
    )
    case Req.put(req, finch: DmarcFinchPool) do
      {:ok, result} when result.status >= 200 and result.status < 300 ->
        {:reply, {:ok, result.body}, state}
    end
  end
end

