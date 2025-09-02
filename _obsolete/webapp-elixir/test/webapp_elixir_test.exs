defmodule WebappElixirTest do
  use ExUnit.Case
  doctest WebappElixir

  test "greets the world" do
    assert WebappElixir.hello() == :world
  end
end
