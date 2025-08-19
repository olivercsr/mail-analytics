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

    try do
    with {:ok, file_contents} <- File.read(mailfilepath),
      mail_msg <- Mail.parse(file_contents) do
      IO.puts("------------------------------- get recipients #{mailfilepath}")
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
    rescue
      e -> IO.puts("wooooooooooooooooooooooops #{mailfilepath} #{inspect e}")
        raise e
    end

    :ok
  end

  def decode_async(config, filepath, donefilepath, filesubdir) do
    IO.puts("=========================== mail decode_async start: #{filepath}")
    # fn -> decode(config, filepath, donefilepath, filesubdir) end
    #   |> Task.async
    task = Task.Supervisor.start_child(
    # task = Task.Supervisor.async(
      TaskSupervisor,
      fn -> decode(config, filepath, donefilepath, filesubdir) end
    )
    IO.puts("=========================== mail decode_async end: #{filepath} #{task}")
    # Task.await(task)
    task
  end
end

