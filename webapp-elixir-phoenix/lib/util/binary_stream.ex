defmodule Util.BinaryStream do
  defp next_fn(<<head::binary-size(128), rest::binary>>) do
    IO.puts("next_fn2 #{inspect head}")
    {:binary.bin_to_list(head), rest}
  end

  defp next_fn(<<b, rest::binary>>) do
    IO.puts("next_fn1 #{b}")
    {[b], rest}
  end

  defp next_fn(<<>>) do
    IO.puts("next_fn0")
    {:halt, <<>>}
  end

  def new(binary_data) do
    Stream.resource(
      fn -> binary_data end,
      &next_fn/1,
      fn res -> res end
    )
  end
end

