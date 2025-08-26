defmodule Dmarc.Recipients do
  @moduledoc """
  The Recipients context.
  """

  import Ecto.Query, warn: false
  alias Dmarc.Repo

  alias Dmarc.Recipients.Recipient
  alias Dmarc.CustomerAccounts.Scope

  @doc """
  Subscribes to scoped notifications about any recipient changes.

  The broadcasted messages match the pattern:

    * {:created, %Recipient{}}
    * {:updated, %Recipient{}}
    * {:deleted, %Recipient{}}

  """
  def subscribe_recipients(%Scope{} = scope) do
    key = scope.customer.id

    Phoenix.PubSub.subscribe(Dmarc.PubSub, "customer:#{key}:recipients")
  end

  defp broadcast(%Scope{} = scope, message) do
    key = scope.customer.id

    Phoenix.PubSub.broadcast(Dmarc.PubSub, "customer:#{key}:recipients", message)
  end

  @doc """
  Returns the list of recipients.

  ## Examples

      iex> list_recipients(scope)
      [%Recipient{}, ...]

  """
  def list_recipients(%Scope{} = scope) do
    Repo.all_by(Recipient, customer_id: scope.customer.id)
  end

  @doc """
  Gets a single recipient.

  Raises `Ecto.NoResultsError` if the Recipient does not exist.

  ## Examples

      iex> get_recipient!(scope, 123)
      %Recipient{}

      iex> get_recipient!(scope, 456)
      ** (Ecto.NoResultsError)

  """
  def get_recipient!(%Scope{} = scope, id) do
    Repo.get_by!(Recipient, id: id, customer_id: scope.customer.id)
  end

  @doc """
  Creates a recipient.

  ## Examples

      iex> create_recipient(scope, %{field: value})
      {:ok, %Recipient{}}

      iex> create_recipient(scope, %{field: bad_value})
      {:error, %Ecto.Changeset{}}

  """
  def create_recipient(%Scope{} = scope, attrs) do
    with {:ok, recipient = %Recipient{}} <-
           %Recipient{}
           |> Recipient.changeset(attrs, scope)
           |> Repo.insert() do
      broadcast(scope, {:created, recipient})
      {:ok, recipient}
    end
  end

  @doc """
  Updates a recipient.

  ## Examples

      iex> update_recipient(scope, recipient, %{field: new_value})
      {:ok, %Recipient{}}

      iex> update_recipient(scope, recipient, %{field: bad_value})
      {:error, %Ecto.Changeset{}}

  """
  def update_recipient(%Scope{} = scope, %Recipient{} = recipient, attrs) do
    true = recipient.customer_id == scope.customer.id

    with {:ok, recipient = %Recipient{}} <-
           recipient
           |> Recipient.changeset(attrs, scope)
           |> Repo.update() do
      broadcast(scope, {:updated, recipient})
      {:ok, recipient}
    end
  end

  @doc """
  Deletes a recipient.

  ## Examples

      iex> delete_recipient(scope, recipient)
      {:ok, %Recipient{}}

      iex> delete_recipient(scope, recipient)
      {:error, %Ecto.Changeset{}}

  """
  def delete_recipient(%Scope{} = scope, %Recipient{} = recipient) do
    true = recipient.customer_id == scope.customer.id

    with {:ok, recipient = %Recipient{}} <-
           Repo.delete(recipient) do
      broadcast(scope, {:deleted, recipient})
      {:ok, recipient}
    end
  end

  @doc """
  Returns an `%Ecto.Changeset{}` for tracking recipient changes.

  ## Examples

      iex> change_recipient(scope, recipient)
      %Ecto.Changeset{data: %Recipient{}}

  """
  def change_recipient(%Scope{} = scope, %Recipient{} = recipient, attrs \\ %{}) do
    true = recipient.customer_id == scope.customer.id

    Recipient.changeset(recipient, attrs, scope)
  end
end
