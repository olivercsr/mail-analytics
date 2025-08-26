defmodule Dmarc.Repo.Migrations.CreateRecipients do
  use Ecto.Migration

  def change do
    create table(:recipients) do
      add :email, :string
      add :tenant_id, references(:tenants, on_delete: :nothing)
      add :customer_id, references(:customers, type: :id, on_delete: :delete_all)

      timestamps(type: :utc_datetime)
    end

    create index(:recipients, [:customer_id])

    create unique_index(:recipients, [:email])
    create index(:recipients, [:tenant_id])
  end
end
