defmodule DmarcWeb.RecipientLiveTest do
  use DmarcWeb.ConnCase

  import Phoenix.LiveViewTest
  import Dmarc.RecipientsFixtures

  @create_attrs %{email: "some email"}
  @update_attrs %{email: "some updated email"}
  @invalid_attrs %{email: nil}

  setup :register_and_log_in_customer

  defp create_recipient(%{scope: scope}) do
    recipient = recipient_fixture(scope)

    %{recipient: recipient}
  end

  describe "Index" do
    setup [:create_recipient]

    test "lists all recipients", %{conn: conn, recipient: recipient} do
      {:ok, _index_live, html} = live(conn, ~p"/recipients")

      assert html =~ "Listing Recipients"
      assert html =~ recipient.email
    end

    test "saves new recipient", %{conn: conn} do
      {:ok, index_live, _html} = live(conn, ~p"/recipients")

      assert {:ok, form_live, _} =
               index_live
               |> element("a", "New Recipient")
               |> render_click()
               |> follow_redirect(conn, ~p"/recipients/new")

      assert render(form_live) =~ "New Recipient"

      assert form_live
             |> form("#recipient-form", recipient: @invalid_attrs)
             |> render_change() =~ "can&#39;t be blank"

      assert {:ok, index_live, _html} =
               form_live
               |> form("#recipient-form", recipient: @create_attrs)
               |> render_submit()
               |> follow_redirect(conn, ~p"/recipients")

      html = render(index_live)
      assert html =~ "Recipient created successfully"
      assert html =~ "some email"
    end

    test "updates recipient in listing", %{conn: conn, recipient: recipient} do
      {:ok, index_live, _html} = live(conn, ~p"/recipients")

      assert {:ok, form_live, _html} =
               index_live
               |> element("#recipients-#{recipient.id} a", "Edit")
               |> render_click()
               |> follow_redirect(conn, ~p"/recipients/#{recipient}/edit")

      assert render(form_live) =~ "Edit Recipient"

      assert form_live
             |> form("#recipient-form", recipient: @invalid_attrs)
             |> render_change() =~ "can&#39;t be blank"

      assert {:ok, index_live, _html} =
               form_live
               |> form("#recipient-form", recipient: @update_attrs)
               |> render_submit()
               |> follow_redirect(conn, ~p"/recipients")

      html = render(index_live)
      assert html =~ "Recipient updated successfully"
      assert html =~ "some updated email"
    end

    test "deletes recipient in listing", %{conn: conn, recipient: recipient} do
      {:ok, index_live, _html} = live(conn, ~p"/recipients")

      assert index_live |> element("#recipients-#{recipient.id} a", "Delete") |> render_click()
      refute has_element?(index_live, "#recipients-#{recipient.id}")
    end
  end

  describe "Show" do
    setup [:create_recipient]

    test "displays recipient", %{conn: conn, recipient: recipient} do
      {:ok, _show_live, html} = live(conn, ~p"/recipients/#{recipient}")

      assert html =~ "Show Recipient"
      assert html =~ recipient.email
    end

    test "updates recipient and returns to show", %{conn: conn, recipient: recipient} do
      {:ok, show_live, _html} = live(conn, ~p"/recipients/#{recipient}")

      assert {:ok, form_live, _} =
               show_live
               |> element("a", "Edit")
               |> render_click()
               |> follow_redirect(conn, ~p"/recipients/#{recipient}/edit?return_to=show")

      assert render(form_live) =~ "Edit Recipient"

      assert form_live
             |> form("#recipient-form", recipient: @invalid_attrs)
             |> render_change() =~ "can&#39;t be blank"

      assert {:ok, show_live, _html} =
               form_live
               |> form("#recipient-form", recipient: @update_attrs)
               |> render_submit()
               |> follow_redirect(conn, ~p"/recipients/#{recipient}")

      html = render(show_live)
      assert html =~ "Recipient updated successfully"
      assert html =~ "some updated email"
    end
  end
end
