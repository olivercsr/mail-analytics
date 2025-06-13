  defmodule Attachment do
    defstruct [
      :filename,
      :transfer_encoding,
      :content_type,
      :content_charset,
      :data
    ]
  end

