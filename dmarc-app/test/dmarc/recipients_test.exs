defmodule Dmarc.RecipientsTest do
  use Dmarc.DataCase

  alias Dmarc.Recipients

  describe "recipients" do
    alias Dmarc.Recipients.Recipient

    import Dmarc.CustomerAccountsFixtures, only: [customer_scope_fixture: 0]
    import Dmarc.RecipientsFixtures

    @invalid_attrs %{email: nil}

    test "list_recipients/1 returns all scoped recipients" do
      scope = customer_scope_fixture()
      other_scope = customer_scope_fixture()
      recipient = recipient_fixture(scope)
      other_recipient = recipient_fixture(other_scope)
      assert Recipients.list_recipients(scope) == [recipient]
      assert Recipients.list_recipients(other_scope) == [other_recipient]
    end

    test "get_recipient!/2 returns the recipient with given id" do
      scope = customer_scope_fixture()
      recipient = recipient_fixture(scope)
      other_scope = customer_scope_fixture()
      assert Recipients.get_recipient!(scope, recipient.id) == recipient
      assert_raise Ecto.NoResultsError, fn -> Recipients.get_recipient!(other_scope, recipient.id) end
    end

    test "create_recipient/2 with valid data creates a recipient" do
      valid_attrs = %{email: "some email"}
      scope = customer_scope_fixture()

      assert {:ok, %Recipient{} = recipient} = Recipients.create_recipient(scope, valid_attrs)
      assert recipient.email == "some email"
      assert recipient.customer_id == scope.customer.id
    end

    test "create_recipient/2 with invalid data returns error changeset" do
      scope = customer_scope_fixture()
      assert {:error, %Ecto.Changeset{}} = Recipients.create_recipient(scope, @invalid_attrs)
    end

    test "update_recipient/3 with valid data updates the recipient" do
      scope = customer_scope_fixture()
      recipient = recipient_fixture(scope)
      update_attrs = %{email: "some updated email"}

      assert {:ok, %Recipient{} = recipient} = Recipients.update_recipient(scope, recipient, update_attrs)
      assert recipient.email == "some updated email"
    end

    test "update_recipient/3 with invalid scope raises" do
      scope = customer_scope_fixture()
      other_scope = customer_scope_fixture()
      recipient = recipient_fixture(scope)

      assert_raise MatchError, fn ->
        Recipients.update_recipient(other_scope, recipient, %{})
      end
    end

    test "update_recipient/3 with invalid data returns error changeset" do
      scope = customer_scope_fixture()
      recipient = recipient_fixture(scope)
      assert {:error, %Ecto.Changeset{}} = Recipients.update_recipient(scope, recipient, @invalid_attrs)
      assert recipient == Recipients.get_recipient!(scope, recipient.id)
    end

    test "delete_recipient/2 deletes the recipient" do
      scope = customer_scope_fixture()
      recipient = recipient_fixture(scope)
      assert {:ok, %Recipient{}} = Recipients.delete_recipient(scope, recipient)
      assert_raise Ecto.NoResultsError, fn -> Recipients.get_recipient!(scope, recipient.id) end
    end

    test "delete_recipient/2 with invalid scope raises" do
      scope = customer_scope_fixture()
      other_scope = customer_scope_fixture()
      recipient = recipient_fixture(scope)
      assert_raise MatchError, fn -> Recipients.delete_recipient(other_scope, recipient) end
    end

    test "change_recipient/2 returns a recipient changeset" do
      scope = customer_scope_fixture()
      recipient = recipient_fixture(scope)
      assert %Ecto.Changeset{} = Recipients.change_recipient(scope, recipient)
    end
  end
end
