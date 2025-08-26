defmodule Dmarc.Repo.Migrations.CreateTenants do
  use Ecto.Migration

  def change do
    create table(:tenants) do
      add :name, :string
      add :customer_id, references(:customers, type: :id, on_delete: :delete_all)

      timestamps(type: :utc_datetime)
    end

    create index(:tenants, [:customer_id])
  end
end
