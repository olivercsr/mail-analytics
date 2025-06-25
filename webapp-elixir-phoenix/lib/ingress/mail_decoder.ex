defmodule Ingress.MailDecoder do
  use GenServer

  require Logger
  require File
  require Mail

  # Client

  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts, name: opts[:name])
  end

  def decode(pid, filepath, donefilepath, filesubdir) do
    GenServer.cast(pid, {:decode, filepath, donefilepath, filesubdir})
  end

  # Server

  # defp search_msg(mail_msg, attachments) do
  #   with dispositions <- get_in(mail_msg.headers["content-disposition"]) || [],
  #     disposition <- Enum.at(dispositions, 0, "")
  #       |> String.downcase() do
  #     case {mail_msg, disposition} do
  #       {%{multipart: false}, "attachment"} ->
  #         [mail_msg | attachments]
  #       {%{multipart: false}, _} ->
  #         attachments
  #       {%{multipart: true}, _} ->
  #         find_attachments(mail_msg.parts || [])
  #       _ -> Logger.warning([message: "unexpected mail message structure", mail_msg: mail_msg])
  #         attachments
  #     end
  #   end
  # end

  # defp get_value_from_param_header(header, key, default) do
  #   header
  #     |> Enum.at(1, {})
  #     |> Tuple.to_list()
  #     |> Enum.chunk_every(2)
  #     |> Enum.filter(fn [k, _] -> String.downcase(k || "") == key end)
  #     |> Enum.map(fn [_, v] -> v end)
  #     |> Enum.at(0, default)
  # end
  #
  # defp default_filename() do
  #   # TODO: implement
  #   "defaultfilename"
  # end
  #
  # defp to_attachment(mail_msg) do
  #   with headers <- get_in(mail_msg.headers),
  #     disposition_header <- get_in(headers["content-disposition"]) || [],
  #     filename <- get_value_from_param_header(
  #       disposition_header,
  #       "filename",
  #       default_filename()
  #     ),
  #     transfer_encoding <- get_in(headers["transfer-encoding"]) || "base64",
  #     content_type_header <- get_in(headers["content-type"]),
  #     content_type <- Enum.at(content_type_header, 0),
  #     content_charset <- get_value_from_param_header(
  #       content_type_header,
  #       "charset",
  #       "utf8"
  #     ),
  #     data <- get_in(mail_msg.body) do
  #     %Attachment{
  #       filename: filename,
  #       transfer_encoding: String.to_atom(transfer_encoding),
  #       content_type: String.to_atom(content_type),
  #       content_charset: String.to_atom(content_charset),
  #       # data: Util.BinaryStream.from_binary(data)
  #       data: data
  #     }
  #   end
  # end

  # defp find_attachments(mail_msgs) do
  #   mail_msgs
  #   |> Enum.reduce([], &search_msg/2)
  # end

  # defp move_file(basepath, srcpath, destpath) do
  #   src = "#{basepath}/#{srcpath}"
  #   dest = "#{basepath}/#{destpath}"
  #   File.rename(src, dest)
  #   dest
  # end

  defp get_tenant_from_recipient(recipient, state) do
    re = state.recipient_regex
    [_all, tenant] = Regex.run(re, recipient)
    case tenant |> String.trim() do
      tenant when is_binary(tenant) and tenant != "" and tenant != nil -> tenant
    end
  end

  @impl true
  def init(opts) do
    {:ok, %{
      recipient_regex: ~r"^([^@\s]+)@.+$",
      opts: opts
    }}
  end

  @impl true
  def handle_cast({:decode, mailfilepath, maildonefilepath, _filesubdir}, state) do
    Logger.debug([module: __MODULE__, message: "MailDecoder.decode start", mailfilepath: mailfilepath, maildonefilepath: maildonefilepath])

    basepath = Path.absname(state.opts[:basepath])
    attachmentsdir = Path.absname("#{basepath}/#{state.opts[:attachmentsdir]}")

    with {:ok, file_contents} <- File.read(mailfilepath),
      mail_msg <- Mail.parse(file_contents) do
      tenant = Mail.get_to(mail_msg)
        |> Enum.at(0)
        |> get_tenant_from_recipient(state)
      results = Mail.get_attachments(mail_msg, :attachment)
        |> Enum.map(fn {attachmentfilename, attachmentdata} ->
          try do
            destdir = Path.absname("#{attachmentsdir}/#{tenant}")
            :ok = File.mkdir_p(destdir)
            attachmentfilepath = Path.absname("#{destdir}/#{attachmentfilename}")
            :ok = File.write(attachmentfilepath, attachmentdata, [:write])
            {:ok, attachmentfilename}
          rescue
            e -> {:error, attachmentfilename, e}
          end
        end)

      :ok = File.rename(mailfilepath, maildonefilepath)

      Logger.debug([module: __MODULE__, message: "MailDecoder.decode done", results: results])
    end

    {:noreply, state}
  end
end

