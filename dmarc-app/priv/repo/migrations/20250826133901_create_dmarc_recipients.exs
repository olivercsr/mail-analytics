defmodule Dmarc.Repo.Migrations.CreateDmarcRecipients do
  use Ecto.Migration

  def change do
    create table(:dmarc_recipients) do
      add :email, :string
      add :tenant_id, references(:tenants, on_delete: :delete_all)
      add :customer_id, references(:customers, type: :id, on_delete: :delete_all)

      timestamps(type: :utc_datetime)
    end

    create index(:dmarc_recipients, [:customer_id])

    create unique_index(:dmarc_recipients, [:email])
    create index(:dmarc_recipients, [:tenant_id])
  end
end
