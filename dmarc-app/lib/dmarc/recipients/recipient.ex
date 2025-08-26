defmodule Dmarc.Recipients.Recipient do
  use Ecto.Schema
  import Ecto.Changeset

  schema "recipients" do
    field :email, :string
    field :tenant_id, :id
    field :customer_id, :id

    timestamps(type: :utc_datetime)
  end

  @doc false
  def changeset(recipient, attrs, customer_scope) do
    recipient
    |> cast(attrs, [:email])
    |> validate_required([:email])
    |> unique_constraint(:email)
    |> put_change(:customer_id, customer_scope.customer.id)
  end
end
