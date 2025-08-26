defmodule Dmarc.Tenants do
  @moduledoc """
  The Tenants context.
  """

  import Ecto.Query, warn: false
  alias Dmarc.Repo

  alias Dmarc.Tenants.Tenant
  alias Dmarc.CustomerAccounts.Scope

  @doc """
  Subscribes to scoped notifications about any tenant changes.

  The broadcasted messages match the pattern:

    * {:created, %Tenant{}}
    * {:updated, %Tenant{}}
    * {:deleted, %Tenant{}}

  """
  def subscribe_tenants(%Scope{} = scope) do
    key = scope.customer.id

    Phoenix.PubSub.subscribe(Dmarc.PubSub, "customer:#{key}:tenants")
  end

  defp broadcast(%Scope{} = scope, message) do
    key = scope.customer.id

    Phoenix.PubSub.broadcast(Dmarc.PubSub, "customer:#{key}:tenants", message)
  end

  @doc """
  Returns the list of tenants.

  ## Examples

      iex> list_tenants(scope)
      [%Tenant{}, ...]

  """
  def list_tenants(%Scope{} = scope) do
    Repo.all_by(Tenant, customer_id: scope.customer.id)
  end

  @doc """
  Gets a single tenant.

  Raises `Ecto.NoResultsError` if the Tenant does not exist.

  ## Examples

      iex> get_tenant!(scope, 123)
      %Tenant{}

      iex> get_tenant!(scope, 456)
      ** (Ecto.NoResultsError)

  """
  def get_tenant!(%Scope{} = scope, id) do
    Repo.get_by!(Tenant, id: id, customer_id: scope.customer.id)
  end

  @doc """
  Creates a tenant.

  ## Examples

      iex> create_tenant(scope, %{field: value})
      {:ok, %Tenant{}}

      iex> create_tenant(scope, %{field: bad_value})
      {:error, %Ecto.Changeset{}}

  """
  def create_tenant(%Scope{} = scope, attrs) do
    with {:ok, tenant = %Tenant{}} <-
           %Tenant{}
           |> Tenant.changeset(attrs, scope)
           |> Repo.insert() do
      broadcast(scope, {:created, tenant})
      {:ok, tenant}
    end
  end

  @doc """
  Updates a tenant.

  ## Examples

      iex> update_tenant(scope, tenant, %{field: new_value})
      {:ok, %Tenant{}}

      iex> update_tenant(scope, tenant, %{field: bad_value})
      {:error, %Ecto.Changeset{}}

  """
  def update_tenant(%Scope{} = scope, %Tenant{} = tenant, attrs) do
    true = tenant.customer_id == scope.customer.id

    with {:ok, tenant = %Tenant{}} <-
           tenant
           |> Tenant.changeset(attrs, scope)
           |> Repo.update() do
      broadcast(scope, {:updated, tenant})
      {:ok, tenant}
    end
  end

  @doc """
  Deletes a tenant.

  ## Examples

      iex> delete_tenant(scope, tenant)
      {:ok, %Tenant{}}

      iex> delete_tenant(scope, tenant)
      {:error, %Ecto.Changeset{}}

  """
  def delete_tenant(%Scope{} = scope, %Tenant{} = tenant) do
    true = tenant.customer_id == scope.customer.id

    with {:ok, tenant = %Tenant{}} <-
           Repo.delete(tenant) do
      broadcast(scope, {:deleted, tenant})
      {:ok, tenant}
    end
  end

  @doc """
  Returns an `%Ecto.Changeset{}` for tracking tenant changes.

  ## Examples

      iex> change_tenant(scope, tenant)
      %Ecto.Changeset{data: %Tenant{}}

  """
  def change_tenant(%Scope{} = scope, %Tenant{} = tenant, attrs \\ %{}) do
    true = tenant.customer_id == scope.customer.id

    Tenant.changeset(tenant, attrs, scope)
  end
end
