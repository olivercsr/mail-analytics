defmodule DmarcWeb.CustomerSessionControllerTest do
  use DmarcWeb.ConnCase, async: true

  import Dmarc.CustomerAccountsFixtures

  setup do
    %{customer: customer_fixture()}
  end

  describe "POST /customers/log_in" do
    test "logs the customer in", %{conn: conn, customer: customer} do
      conn =
        post(conn, ~p"/customers/log_in", %{
          "customer" => %{"email" => customer.email, "password" => valid_customer_password()}
        })

      assert get_session(conn, :customer_token)
      assert redirected_to(conn) == ~p"/"

      # Now do a logged in request and assert on the menu
      conn = get(conn, ~p"/")
      response = html_response(conn, 200)
      assert response =~ customer.email
      assert response =~ ~p"/customers/settings"
      assert response =~ ~p"/customers/log_out"
    end

    test "logs the customer in with remember me", %{conn: conn, customer: customer} do
      conn =
        post(conn, ~p"/customers/log_in", %{
          "customer" => %{
            "email" => customer.email,
            "password" => valid_customer_password(),
            "remember_me" => "true"
          }
        })

      assert conn.resp_cookies["_dmarc_web_customer_remember_me"]
      assert redirected_to(conn) == ~p"/"
    end

    test "logs the customer in with return to", %{conn: conn, customer: customer} do
      conn =
        conn
        |> init_test_session(customer_return_to: "/foo/bar")
        |> post(~p"/customers/log_in", %{
          "customer" => %{
            "email" => customer.email,
            "password" => valid_customer_password()
          }
        })

      assert redirected_to(conn) == "/foo/bar"
      assert Phoenix.Flash.get(conn.assigns.flash, :info) =~ "Welcome back!"
    end

    test "login following registration", %{conn: conn, customer: customer} do
      conn =
        conn
        |> post(~p"/customers/log_in", %{
          "_action" => "registered",
          "customer" => %{
            "email" => customer.email,
            "password" => valid_customer_password()
          }
        })

      assert redirected_to(conn) == ~p"/"
      assert Phoenix.Flash.get(conn.assigns.flash, :info) =~ "Account created successfully"
    end

    test "login following password update", %{conn: conn, customer: customer} do
      conn =
        conn
        |> post(~p"/customers/log_in", %{
          "_action" => "password_updated",
          "customer" => %{
            "email" => customer.email,
            "password" => valid_customer_password()
          }
        })

      assert redirected_to(conn) == ~p"/customers/settings"
      assert Phoenix.Flash.get(conn.assigns.flash, :info) =~ "Password updated successfully"
    end

    test "redirects to login page with invalid credentials", %{conn: conn} do
      conn =
        post(conn, ~p"/customers/log_in", %{
          "customer" => %{"email" => "invalid@email.com", "password" => "invalid_password"}
        })

      assert Phoenix.Flash.get(conn.assigns.flash, :error) == "Invalid email or password"
      assert redirected_to(conn) == ~p"/customers/log_in"
    end
  end

  describe "DELETE /customers/log_out" do
    test "logs the customer out", %{conn: conn, customer: customer} do
      conn = conn |> log_in_customer(customer) |> delete(~p"/customers/log_out")
      assert redirected_to(conn) == ~p"/"
      refute get_session(conn, :customer_token)
      assert Phoenix.Flash.get(conn.assigns.flash, :info) =~ "Logged out successfully"
    end

    test "succeeds even if the customer is not logged in", %{conn: conn} do
      conn = delete(conn, ~p"/customers/log_out")
      assert redirected_to(conn) == ~p"/"
      refute get_session(conn, :customer_token)
      assert Phoenix.Flash.get(conn.assigns.flash, :info) =~ "Logged out successfully"
    end
  end
end
