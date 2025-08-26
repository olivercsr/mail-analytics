defmodule Dmarc.TenantsFixtures do
  @moduledoc """
  This module defines test helpers for creating
  entities via the `Dmarc.Tenants` context.
  """

  @doc """
  Generate a tenant.
  """
  def tenant_fixture(scope, attrs \\ %{}) do
    attrs =
      Enum.into(attrs, %{
        name: "some name"
      })

    {:ok, tenant} = Dmarc.Tenants.create_tenant(scope, attrs)
    tenant
  end
end
