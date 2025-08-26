defmodule Dmarc.RecipientsFixtures do
  @moduledoc """
  This module defines test helpers for creating
  entities via the `Dmarc.Recipients` context.
  """

  @doc """
  Generate a unique recipient email.
  """
  def unique_recipient_email, do: "some email#{System.unique_integer([:positive])}"

  @doc """
  Generate a recipient.
  """
  def recipient_fixture(scope, attrs \\ %{}) do
    attrs =
      Enum.into(attrs, %{
        email: unique_recipient_email()
      })

    {:ok, recipient} = Dmarc.Recipients.create_recipient(scope, attrs)
    recipient
  end
end
