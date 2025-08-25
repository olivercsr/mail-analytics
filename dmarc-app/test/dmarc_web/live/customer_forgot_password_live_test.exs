defmodule DmarcWeb.CustomerForgotPasswordLiveTest do
  use DmarcWeb.ConnCase, async: true

  import Phoenix.LiveViewTest
  import Dmarc.CustomerAccountsFixtures

  alias Dmarc.CustomerAccounts
  alias Dmarc.Repo

  describe "Forgot password page" do
    test "renders email page", %{conn: conn} do
      {:ok, lv, html} = live(conn, ~p"/customers/reset_password")

      assert html =~ "Forgot your password?"
      assert has_element?(lv, ~s|a[href="#{~p"/customers/register"}"]|, "Register")
      assert has_element?(lv, ~s|a[href="#{~p"/customers/log_in"}"]|, "Log in")
    end

    test "redirects if already logged in", %{conn: conn} do
      result =
        conn
        |> log_in_customer(customer_fixture())
        |> live(~p"/customers/reset_password")
        |> follow_redirect(conn, ~p"/")

      assert {:ok, _conn} = result
    end
  end

  describe "Reset link" do
    setup do
      %{customer: customer_fixture()}
    end

    test "sends a new reset password token", %{conn: conn, customer: customer} do
      {:ok, lv, _html} = live(conn, ~p"/customers/reset_password")

      {:ok, conn} =
        lv
        |> form("#reset_password_form", customer: %{"email" => customer.email})
        |> render_submit()
        |> follow_redirect(conn, "/")

      assert Phoenix.Flash.get(conn.assigns.flash, :info) =~ "If your email is in our system"

      assert Repo.get_by!(CustomerAccounts.CustomerToken, customer_id: customer.id).context ==
               "reset_password"
    end

    test "does not send reset password token if email is invalid", %{conn: conn} do
      {:ok, lv, _html} = live(conn, ~p"/customers/reset_password")

      {:ok, conn} =
        lv
        |> form("#reset_password_form", customer: %{"email" => "unknown@example.com"})
        |> render_submit()
        |> follow_redirect(conn, "/")

      assert Phoenix.Flash.get(conn.assigns.flash, :info) =~ "If your email is in our system"
      assert Repo.all(CustomerAccounts.CustomerToken) == []
    end
  end
end
