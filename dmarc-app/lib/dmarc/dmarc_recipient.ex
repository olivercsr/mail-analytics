defmodule Dmarc.DmarcRecipient do
  use Ecto.Schema
  import Ecto.Changeset

  schema "dmarc_recipients" do
    field :email, :string
    field :tenant_id, :id
    field :customer_id, :id

    timestamps(type: :utc_datetime)
  end

  @doc false
  def changeset(dmarc_recipient, attrs, customer_scope) do
    dmarc_recipient
    |> cast(attrs, [:email])
    |> validate_required([:email])
    |> unique_constraint(:email)
    |> put_change(:customer_id, customer_scope.customer.id)
  end
end
