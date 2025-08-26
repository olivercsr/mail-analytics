defmodule Ingress.MailDecoder do
  require Logger
  require File
  require Mail

  defmodule Config do
    defstruct [:basepath, :attachmentsdir]
  end

  def decode(config, mailfilepath, maildonefilepath, _filesubdir) do
    Logger.debug([module: __MODULE__, message: "MailDecoder.decode start", mailfilepath: mailfilepath, maildonefilepath: maildonefilepath])

    basepath = Path.absname(config.basepath)
    attachmentsdir = Path.absname("#{basepath}/#{config.attachmentsdir}")

    with {:ok, file_contents} <- File.read(mailfilepath),
      mail_msg <- Mail.parse(file_contents) do
      # TODO: allow for multiple recipients and then search for the one that belongs to us
      # TODO: process only our own domains, not 3rd party ones:
      [tenant] = Mail.get_to(mail_msg)
        # |> Enum.at(0)
        # |> get_tenant_from_recipient(state)
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

    :ok
  end
end

