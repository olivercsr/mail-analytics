defmodule Ingress.AttachmentProcessor do
  use GenServer

  # Client

  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts, name: opts[:name])
  end

  def process(pid, attachment) do
    GenServer.cast(pid, {:process, attachment})
  end

  # Server

  @impl true
  def init(opts) do
    {:ok, %{opts: opts}}
  end

  @impl true
  def handle_cast({:process, attachment}, state) do
    IO.puts("handle attachment: #{inspect attachment}")
    # TODO: implement

    {:noreply, state}
  end
end

