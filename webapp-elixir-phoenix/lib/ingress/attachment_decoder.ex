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

  # defp decode_transfer(:base64, data) do
  #   IO.inspect(data)
  #   IO.puts("===============================================1")
  #   # data = "H4sIAAAAAAAEAJVUwY7aMBC9V+o/IO7FCYFNibzeXnpr1UMvvUXGngSLxLZsB7Z/3wlxQoCtur2A/ebNzJtnO/TltW0WJ3BeGf28TFfJ8oV9/EArALnn4rjAsPbFq5fPy0MItiDkfD6vztnKuJqskyQlv75/+ykO0PLlRFb/Jn9S2geuBSyx3WJBowSGCigZN5eIA2tcKFsIXPLALyDCWLLUvAX2VQdw1ikPix9daIw5UjIFIxkbqobJljsxlPvSKuGMN1VYCdNSMhAiO3ZUksk832awS3haZZs8X+/2YpdluczyZL3Jsy0lV25MRo1QOq7rsTlie6gVjpZv0nW6Qx8oGZCJAFpewlmyferD/T7WI3cFp5Y3hlBrGiV+l7bbN8of4CrH4GSaCe/Q8cq4lgd1XEnAukMk8rg8qpY5SobFiHpbXcD+P2KWaaMx346AHxE/QVYElvaD9IuIVYYhgL/DFG8KRuuFcZN4Z85Xj7zpnIBSWbbmSVKkm21SbD4/bYp1UhRplmL/iTElCdNplELJsJjw2B1OvOnQXzlFesuUt8ar0N/AYbA5Mif2TlnuPTKupkVPqhi5Ojeb+r4vHuo0KlUSdFCVwkcwvyEnaIyFMhh2VlorXbfmBP5ylPPgY0blTPvWBbglTHkH4BLcX7Pm4VH8o2LKu3AoHfiuCbMp7m165+0cLIUGRDCO1cbUTX/dRmBGGjpG6+Pm6v5Nezo/mf/VItA31vYmoI7L5t0iZk+JPLjU8+MLwLcSP8LsDxJn0uamBQAA"
  #   res = data
  #     # |> Enum.to_list()
  #     # |> List.to_string()
  #     |> Base.decode64()
  #   IO.puts("===============================================2")
  #   IO.inspect(res)
  #   res
  # end

  defp decode_content(:"application/gzip", data) do
    # TODO: uncompress data
  end

  defp decode_charset(:"us-ascii", data) do
    # TODO: convert to utf8
  end

  @impl true
  def init(opts) do
    {:ok, %{opts: opts}}
  end

  @impl true
  def handle_cast({:decode, attachment}, state) do
    # TODO: deocde attachment
    # {:ok, decoded} = decode_transfer(attachment.transfer_encoding, attachment.data)

    # IO.inspect(decoded)

    {:noreply, state}
  end
end

