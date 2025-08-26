defmodule DmarcWeb.TenantLive.Index do
  use DmarcWeb, :live_view

  alias Dmarc.Tenants

  @impl true
  def render(assigns) do
    ~H"""
    <Layouts.app flash={@flash} current_scope={@current_scope}>
      <.header>
        Listing Tenants
        <:actions>
          <.button variant="primary" navigate={~p"/tenants/new"}>
            <.icon name="hero-plus" /> New Tenant
          </.button>
        </:actions>
      </.header>

      <.table
        id="tenants"
        rows={@streams.tenants}
        row_click={fn {_id, tenant} -> JS.navigate(~p"/tenants/#{tenant}") end}
      >
        <:col :let={{_id, tenant}} label="Name">{tenant.name}</:col>
        <:action :let={{_id, tenant}}>
          <div class="sr-only">
            <.link navigate={~p"/tenants/#{tenant}"}>Show</.link>
          </div>
          <.link navigate={~p"/tenants/#{tenant}/edit"}>Edit</.link>
        </:action>
        <:action :let={{id, tenant}}>
          <.link
            phx-click={JS.push("delete", value: %{id: tenant.id}) |> hide("##{id}")}
            data-confirm="Are you sure?"
          >
            Delete
          </.link>
        </:action>
      </.table>
    </Layouts.app>
    """
  end

  @impl true
  def mount(_params, _session, socket) do
    if connected?(socket) do
      Tenants.subscribe_tenants(socket.assigns.current_scope)
    end

    {:ok,
     socket
     |> assign(:page_title, "Listing Tenants")
     |> stream(:tenants, Tenants.list_tenants(socket.assigns.current_scope))}
  end

  @impl true
  def handle_event("delete", %{"id" => id}, socket) do
    tenant = Tenants.get_tenant!(socket.assigns.current_scope, id)
    {:ok, _} = Tenants.delete_tenant(socket.assigns.current_scope, tenant)

    {:noreply, stream_delete(socket, :tenants, tenant)}
  end

  @impl true
  def handle_info({type, %Dmarc.Tenants.Tenant{}}, socket)
      when type in [:created, :updated, :deleted] do
    {:noreply, stream(socket, :tenants, Tenants.list_tenants(socket.assigns.current_scope), reset: true)}
  end
end
