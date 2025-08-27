defmodule Dmarc.CustomerAccounts.Tenant do
  use Ecto.Schema
  import Ecto.Changeset

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
end
