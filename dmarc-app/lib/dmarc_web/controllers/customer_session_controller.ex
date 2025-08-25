defmodule DmarcWeb.CustomerSessionController do
  use DmarcWeb, :controller

  alias Dmarc.CustomerAccounts
  alias DmarcWeb.CustomerAuth

  def create(conn, %{"_action" => "registered"} = params) do
    create(conn, params, "Account created successfully!")
  end

  def create(conn, %{"_action" => "password_updated"} = params) do
    conn
    |> put_session(:customer_return_to, ~p"/customers/settings")
    |> create(params, "Password updated successfully!")
  end

  def create(conn, params) do
    create(conn, params, "Welcome back!")
  end

  defp create(conn, %{"customer" => customer_params}, info) do
    %{"email" => email, "password" => password} = customer_params

    if customer = CustomerAccounts.get_customer_by_email_and_password(email, password) do
      conn
      |> put_flash(:info, info)
      |> CustomerAuth.log_in_customer(customer, customer_params)
    else
      # In order to prevent user enumeration attacks, don't disclose whether the email is registered.
      conn
      |> put_flash(:error, "Invalid email or password")
      |> put_flash(:email, String.slice(email, 0, 160))
      |> redirect(to: ~p"/customers/log_in")
    end
  end

  def delete(conn, _params) do
    conn
    |> put_flash(:info, "Logged out successfully.")
    |> CustomerAuth.log_out_customer()
  end
end
