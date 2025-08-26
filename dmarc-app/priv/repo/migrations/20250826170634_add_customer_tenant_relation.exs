defmodule Dmarc.Repo.Migrations.AddCustomerTenantRelation do
  use Ecto.Migration

  def change do
    alter table(:customers) do
      add :tenant_id, references(:tenants, on_delete: :delete_all), null: false
    end

    create index(:customers, [:tenant_id])
  end
end
