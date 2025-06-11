defmodule Ingress.EmailFetcher do
  require Logger
  require File
  require Mail

  defp search_msg(mail_msg, attachments) do
    with dispositions <- get_in(mail_msg.headers["content-disposition"]) || [],
      disposition <- Enum.at(dispositions, 0, "")
        |> String.downcase() do
      case {mail_msg, disposition} do
        {%{multipart: false}, "attachment"} ->
          [mail_msg | attachments]
        {%{multipart: false}, _} ->
          attachments
        {%{multipart: true}, _} ->
          find_attachments(mail_msg.parts || [])
        _ -> Logger.warning([message: "unexpected mail message structure", mail_msg: mail_msg])
          attachments
      end
    end
  end

  defp find_attachments(mail_msgs) do
    mail_msgs
    |> Enum.reduce([], &search_msg/2)
  end

  def action(file) do
    IO.puts("EmailFetcher: #{file}")

    with {:ok, file_contents} <- File.read(file),
      mail_msg <- Mail.parse(file_contents),
      attachments <- find_attachments([mail_msg]) do
      IO.inspect(attachments)
      {:ok, attachments}
    end
  end
end
