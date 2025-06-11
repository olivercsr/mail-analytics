defmodule EmailFetcher do
  require File
  require Mail

  def action(file) do
    IO.puts("EmailFetcher: #{file}")

    with {:ok, file_contents} <- File.read(file),
      mail_contents <- Mail.parse(file_contents) do
      {:ok, mail_contents}
    end
  end
end
