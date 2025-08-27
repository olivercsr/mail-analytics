defmodule Dmarc.CustomerAccounts.Tenant do
  use Ecto.Schema
  import Ecto.Changeset
  import Ecto.Query

  alias Dmarc.CustomerAccounts.Tenant
  alias Dmarc.Repo

  schema "tenants" do
    field :name, :string
    field :dmarc_email, :string
    # field :customer_id, :id

    timestamps(type: :utc_datetime)
  end

  @doc false
  def changeset(tenant, attrs, _customer_scope) do
    tenant
    |> cast(attrs, [:name, :dmarc_email])
    |> validate_required([:name, :dmarc_email])
    # |> put_change(:customer_id, customer_scope.customer.id)
  end

  def get_by_email(email) do
    Repo.one(from Tenant, where: [dmarc_email: ^email])
  end
end
