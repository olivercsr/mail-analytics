defmodule DmarcWeb.CustomerSettingsLiveTest do
  use DmarcWeb.ConnCase, async: true

  alias Dmarc.CustomerAccounts
  import Phoenix.LiveViewTest
  import Dmarc.CustomerAccountsFixtures

  describe "Settings page" do
    test "renders settings page", %{conn: conn} do
      {:ok, _lv, html} =
        conn
        |> log_in_customer(customer_fixture())
        |> live(~p"/customers/settings")

      assert html =~ "Change Email"
      assert html =~ "Change Password"
    end

    test "redirects if customer is not logged in", %{conn: conn} do
      assert {:error, redirect} = live(conn, ~p"/customers/settings")

      assert {:redirect, %{to: path, flash: flash}} = redirect
      assert path == ~p"/customers/log_in"
      assert %{"error" => "You must log in to access this page."} = flash
    end
  end

  describe "update email form" do
    setup %{conn: conn} do
      password = valid_customer_password()
      customer = customer_fixture(%{password: password})
      %{conn: log_in_customer(conn, customer), customer: customer, password: password}
    end

    test "updates the customer email", %{conn: conn, password: password, customer: customer} do
      new_email = unique_customer_email()

      {:ok, lv, _html} = live(conn, ~p"/customers/settings")

      result =
        lv
        |> form("#email_form", %{
          "current_password" => password,
          "customer" => %{"email" => new_email}
        })
        |> render_submit()

      assert result =~ "A link to confirm your email"
      assert CustomerAccounts.get_customer_by_email(customer.email)
    end

    test "renders errors with invalid data (phx-change)", %{conn: conn} do
      {:ok, lv, _html} = live(conn, ~p"/customers/settings")

      result =
        lv
        |> element("#email_form")
        |> render_change(%{
          "action" => "update_email",
          "current_password" => "invalid",
          "customer" => %{"email" => "with spaces"}
        })

      assert result =~ "Change Email"
      assert result =~ "must have the @ sign and no spaces"
    end

    test "renders errors with invalid data (phx-submit)", %{conn: conn, customer: customer} do
      {:ok, lv, _html} = live(conn, ~p"/customers/settings")

      result =
        lv
        |> form("#email_form", %{
          "current_password" => "invalid",
          "customer" => %{"email" => customer.email}
        })
        |> render_submit()

      assert result =~ "Change Email"
      assert result =~ "did not change"
      assert result =~ "is not valid"
    end
  end

  describe "update password form" do
    setup %{conn: conn} do
      password = valid_customer_password()
      customer = customer_fixture(%{password: password})
      %{conn: log_in_customer(conn, customer), customer: customer, password: password}
    end

    test "updates the customer password", %{conn: conn, customer: customer, password: password} do
      new_password = valid_customer_password()

      {:ok, lv, _html} = live(conn, ~p"/customers/settings")

      form =
        form(lv, "#password_form", %{
          "current_password" => password,
          "customer" => %{
            "email" => customer.email,
            "password" => new_password,
            "password_confirmation" => new_password
          }
        })

      render_submit(form)

      new_password_conn = follow_trigger_action(form, conn)

      assert redirected_to(new_password_conn) == ~p"/customers/settings"

      assert get_session(new_password_conn, :customer_token) != get_session(conn, :customer_token)

      assert Phoenix.Flash.get(new_password_conn.assigns.flash, :info) =~
               "Password updated successfully"

      assert CustomerAccounts.get_customer_by_email_and_password(customer.email, new_password)
    end

    test "renders errors with invalid data (phx-change)", %{conn: conn} do
      {:ok, lv, _html} = live(conn, ~p"/customers/settings")

      result =
        lv
        |> element("#password_form")
        |> render_change(%{
          "current_password" => "invalid",
          "customer" => %{
            "password" => "too short",
            "password_confirmation" => "does not match"
          }
        })

      assert result =~ "Change Password"
      assert result =~ "should be at least 12 character(s)"
      assert result =~ "does not match password"
    end

    test "renders errors with invalid data (phx-submit)", %{conn: conn} do
      {:ok, lv, _html} = live(conn, ~p"/customers/settings")

      result =
        lv
        |> form("#password_form", %{
          "current_password" => "invalid",
          "customer" => %{
            "password" => "too short",
            "password_confirmation" => "does not match"
          }
        })
        |> render_submit()

      assert result =~ "Change Password"
      assert result =~ "should be at least 12 character(s)"
      assert result =~ "does not match password"
      assert result =~ "is not valid"
    end
  end

  describe "confirm email" do
    setup %{conn: conn} do
      customer = customer_fixture()
      email = unique_customer_email()

      token =
        extract_customer_token(fn url ->
          CustomerAccounts.deliver_customer_update_email_instructions(%{customer | email: email}, customer.email, url)
        end)

      %{conn: log_in_customer(conn, customer), token: token, email: email, customer: customer}
    end

    test "updates the customer email once", %{conn: conn, customer: customer, token: token, email: email} do
      {:error, redirect} = live(conn, ~p"/customers/settings/confirm_email/#{token}")

      assert {:live_redirect, %{to: path, flash: flash}} = redirect
      assert path == ~p"/customers/settings"
      assert %{"info" => message} = flash
      assert message == "Email changed successfully."
      refute CustomerAccounts.get_customer_by_email(customer.email)
      assert CustomerAccounts.get_customer_by_email(email)

      # use confirm token again
      {:error, redirect} = live(conn, ~p"/customers/settings/confirm_email/#{token}")
      assert {:live_redirect, %{to: path, flash: flash}} = redirect
      assert path == ~p"/customers/settings"
      assert %{"error" => message} = flash
      assert message == "Email change link is invalid or it has expired."
    end

    test "does not update email with invalid token", %{conn: conn, customer: customer} do
      {:error, redirect} = live(conn, ~p"/customers/settings/confirm_email/oops")
      assert {:live_redirect, %{to: path, flash: flash}} = redirect
      assert path == ~p"/customers/settings"
      assert %{"error" => message} = flash
      assert message == "Email change link is invalid or it has expired."
      assert CustomerAccounts.get_customer_by_email(customer.email)
    end

    test "redirects if customer is not logged in", %{token: token} do
      conn = build_conn()
      {:error, redirect} = live(conn, ~p"/customers/settings/confirm_email/#{token}")
      assert {:redirect, %{to: path, flash: flash}} = redirect
      assert path == ~p"/customers/log_in"
      assert %{"error" => message} = flash
      assert message == "You must log in to access this page."
    end
  end
end
