defmodule Dmarc.TenantsTest do
  use Dmarc.DataCase

  alias Dmarc.Tenants

  describe "tenants" do
    alias Dmarc.Tenants.Tenant

    import Dmarc.CustomerAccountsFixtures, only: [customer_scope_fixture: 0]
    import Dmarc.TenantsFixtures

    @invalid_attrs %{name: nil}

    test "list_tenants/1 returns all scoped tenants" do
      scope = customer_scope_fixture()
      other_scope = customer_scope_fixture()
      tenant = tenant_fixture(scope)
      other_tenant = tenant_fixture(other_scope)
      assert Tenants.list_tenants(scope) == [tenant]
      assert Tenants.list_tenants(other_scope) == [other_tenant]
    end

    test "get_tenant!/2 returns the tenant with given id" do
      scope = customer_scope_fixture()
      tenant = tenant_fixture(scope)
      other_scope = customer_scope_fixture()
      assert Tenants.get_tenant!(scope, tenant.id) == tenant
      assert_raise Ecto.NoResultsError, fn -> Tenants.get_tenant!(other_scope, tenant.id) end
    end

    test "create_tenant/2 with valid data creates a tenant" do
      valid_attrs = %{name: "some name"}
      scope = customer_scope_fixture()

      assert {:ok, %Tenant{} = tenant} = Tenants.create_tenant(scope, valid_attrs)
      assert tenant.name == "some name"
      assert tenant.customer_id == scope.customer.id
    end

    test "create_tenant/2 with invalid data returns error changeset" do
      scope = customer_scope_fixture()
      assert {:error, %Ecto.Changeset{}} = Tenants.create_tenant(scope, @invalid_attrs)
    end

    test "update_tenant/3 with valid data updates the tenant" do
      scope = customer_scope_fixture()
      tenant = tenant_fixture(scope)
      update_attrs = %{name: "some updated name"}

      assert {:ok, %Tenant{} = tenant} = Tenants.update_tenant(scope, tenant, update_attrs)
      assert tenant.name == "some updated name"
    end

    test "update_tenant/3 with invalid scope raises" do
      scope = customer_scope_fixture()
      other_scope = customer_scope_fixture()
      tenant = tenant_fixture(scope)

      assert_raise MatchError, fn ->
        Tenants.update_tenant(other_scope, tenant, %{})
      end
    end

    test "update_tenant/3 with invalid data returns error changeset" do
      scope = customer_scope_fixture()
      tenant = tenant_fixture(scope)
      assert {:error, %Ecto.Changeset{}} = Tenants.update_tenant(scope, tenant, @invalid_attrs)
      assert tenant == Tenants.get_tenant!(scope, tenant.id)
    end

    test "delete_tenant/2 deletes the tenant" do
      scope = customer_scope_fixture()
      tenant = tenant_fixture(scope)
      assert {:ok, %Tenant{}} = Tenants.delete_tenant(scope, tenant)
      assert_raise Ecto.NoResultsError, fn -> Tenants.get_tenant!(scope, tenant.id) end
    end

    test "delete_tenant/2 with invalid scope raises" do
      scope = customer_scope_fixture()
      other_scope = customer_scope_fixture()
      tenant = tenant_fixture(scope)
      assert_raise MatchError, fn -> Tenants.delete_tenant(other_scope, tenant) end
    end

    test "change_tenant/2 returns a tenant changeset" do
      scope = customer_scope_fixture()
      tenant = tenant_fixture(scope)
      assert %Ecto.Changeset{} = Tenants.change_tenant(scope, tenant)
    end
  end
end
