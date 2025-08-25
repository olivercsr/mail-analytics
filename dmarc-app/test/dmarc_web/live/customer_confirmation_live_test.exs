defmodule DmarcWeb.CustomerConfirmationLiveTest do
  use DmarcWeb.ConnCase, async: true

  import Phoenix.LiveViewTest
  import Dmarc.CustomerAccountsFixtures

  alias Dmarc.CustomerAccounts
  alias Dmarc.Repo

  setup do
    %{customer: customer_fixture()}
  end

  describe "Confirm customer" do
    test "renders confirmation page", %{conn: conn} do
      {:ok, _lv, html} = live(conn, ~p"/customers/confirm/some-token")
      assert html =~ "Confirm Account"
    end

    test "confirms the given token once", %{conn: conn, customer: customer} do
      token =
        extract_customer_token(fn url ->
          CustomerAccounts.deliver_customer_confirmation_instructions(customer, url)
        end)

      {:ok, lv, _html} = live(conn, ~p"/customers/confirm/#{token}")

      result =
        lv
        |> form("#confirmation_form")
        |> render_submit()
        |> follow_redirect(conn, "/")

      assert {:ok, conn} = result

      assert Phoenix.Flash.get(conn.assigns.flash, :info) =~
               "Customer confirmed successfully"

      assert CustomerAccounts.get_customer!(customer.id).confirmed_at
      refute get_session(conn, :customer_token)
      assert Repo.all(CustomerAccounts.CustomerToken) == []

      # when not logged in
      {:ok, lv, _html} = live(conn, ~p"/customers/confirm/#{token}")

      result =
        lv
        |> form("#confirmation_form")
        |> render_submit()
        |> follow_redirect(conn, "/")

      assert {:ok, conn} = result

      assert Phoenix.Flash.get(conn.assigns.flash, :error) =~
               "Customer confirmation link is invalid or it has expired"

      # when logged in
      conn =
        build_conn()
        |> log_in_customer(customer)

      {:ok, lv, _html} = live(conn, ~p"/customers/confirm/#{token}")

      result =
        lv
        |> form("#confirmation_form")
        |> render_submit()
        |> follow_redirect(conn, "/")

      assert {:ok, conn} = result
      refute Phoenix.Flash.get(conn.assigns.flash, :error)
    end

    test "does not confirm email with invalid token", %{conn: conn, customer: customer} do
      {:ok, lv, _html} = live(conn, ~p"/customers/confirm/invalid-token")

      {:ok, conn} =
        lv
        |> form("#confirmation_form")
        |> render_submit()
        |> follow_redirect(conn, ~p"/")

      assert Phoenix.Flash.get(conn.assigns.flash, :error) =~
               "Customer confirmation link is invalid or it has expired"

      refute CustomerAccounts.get_customer!(customer.id).confirmed_at
    end
  end
end
