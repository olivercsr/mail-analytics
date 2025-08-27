defmodule Dmarc.Dmarc do
  defp charset(), do: "abcdefghijklmnopqrstuvwxyz"
  defp charset_length(), do: String.length(charset())

  def generate_mail_address(domain, length) do
    recipient = Stream.repeatedly(fn -> :rand.uniform(charset_length()) - 1 end)
      |> Enum.take(length)
      |> Enum.map(fn idx -> String.at(charset(), idx) end)
      |> Enum.join()

    "#{recipient}@#{domain}"
  end
end

