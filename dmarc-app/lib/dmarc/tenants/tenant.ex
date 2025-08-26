defmodule Dmarc.Tenants.Tenant do
  use Ecto.Schema
  import Ecto.Changeset

  schema "tenants" do
    field :name, :string
    field :customer_id, :id

    timestamps(type: :utc_datetime)
  end

  @doc false
  def changeset(tenant, attrs, customer_scope) do
    tenant
    |> cast(attrs, [:name])
    |> validate_required([:name])
    |> put_change(:customer_id, customer_scope.customer.id)
  end
end
