defmodule Ingress.AttachmentDecoder do
  use GenServer

  # Client

  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts, name: opts[:name])
  end

  def decode(pid, attachment) do
    GenServer.cast(pid, {:decode, attachment})
  end

  # Server

  defp decode_transfer(:base64, data) do
  end

  @impl true
  def init(opts) do
    {:ok, %{opts: opts}}
  end

  @impl true
  def handle_cast({:decode, attachment}, state) do
    # TODO: deocde attachment

    {:noreply, state}
  end
end

