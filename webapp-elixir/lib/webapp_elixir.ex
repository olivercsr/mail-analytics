defmodule WebappElixir do

  use GenServer

  @moduledoc """
  Documentation for `WebappElixir`.
  """

  @doc """
  Hello world.

  ## Examples

      iex> WebappElixir.hello()
      :world

  """
  def hello do
    :world
  end

  @impl true
  def init(x) do
    {:ok, x}
  end

  @impl true
  def handle_call(:get, _from, x) do
    IO.puts "handle"
    {:reply, x, x}
  end

  #def child_spec(_arg) do
  #  %{
  #    id: WebappElixir,
  #    start: {WebappElixir, :start_link, []}
  #  }
  #end

  def start_link(arg) do
    IO.puts "worker!"
    GenServer.start_link(__MODULE__, arg, name: __MODULE__)
    #Process.sleep(10000)
  end

  def main(arg) do
    IO.puts "main"
  end
end
