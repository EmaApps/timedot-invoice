<!DOCTYPE HTML>
<html lang="en">

<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <title>
    Invoice
    <invoice:number />
    -
    <invoice:metadata>
      <value var="name" />
    </invoice:metadata>
  </title>
  <link href="https://unpkg.com/tailwindcss@2/dist/tailwind.min.css" rel="stylesheet" type="text/css">
</head>

<!-- DoNotFormat -->
<bind tag="theme"><invoice:metadata><value var="theme" /></invoice:metadata></bind>
<bind tag="dollar"><span class="text-blue-700" title="USD">$</span></bind>
<!-- DoNotFormat -->

<body class="overflow-y-scroll">
  <div class="container mx-auto max-w-screen-lg pt-4">
    <header class="text-4xl text-center border-b-2 py-2 mb-4 bg-${theme}-200 ">
      Invoice
      <invoice:number />
      -
      <invoice:metadata>
        <value var="name" />
      </invoice:metadata>
    </header>

    <invoice:errors>
      <error>
        <li class="text-red-500">
          <error:err />
        </li>
      </error>
    </invoice:errors>

    <div class="flex flex-row">
      <section id="me" class="w-2/3">
        Payable to:
        <div class="text-3xl font-bold">
          <invoice:metadata>
            <value var="name" />
          </invoice:metadata>
        </div>
        <address id="address">
          <invoice:metadata>
            <with var="address">
              <div>
                <value />
              </div>
            </with>
          </invoice:metadata>
        </address>
        <hr />
        <div>
          Invoice date:
          <invoice:number />
        </div>

      </section>
      <section id="them" class="">
        Client:
        <div class="text-3xl font-bold">
          <invoice:metadata>
            <value var="payable-to.name" />
          </invoice:metadata>
        </div>
        <address id="address">
          <invoice:metadata>
            <with var="payable-to.address">
              <div>
                <value />
              </div>
            </with>
          </invoice:metadata>
        </address>
        <hr />
        Contact:
        <div class="text-2xl font-bold">
          <invoice:metadata>
            <value var="payable-to.contact.name" />
          </invoice:metadata>
        </div>
        <div class="italic">
          <invoice:metadata>
            <value var="payable-to.contact.phone" />
          </invoice:metadata>
        </div>
        <div class="">
          <invoice:metadata>
            <value var="payable-to.contact.email" />
          </invoice:metadata>
        </div>


      </section>

    </div>

    <div class="relative overflow-x-auto shadow-md sm:rounded-lg my-4">
      <table class="w-full text-left text-gray-700 ">
        <!-- Header -->
        <thead class="text-xs text-gray-700 uppercase bg-gray-50 ">
          <tr>
            <th></th>
            <invoice:clients>
              <invoice:each-client>
                <th class="px-6 py-3">
                  <invoice:client />
                </th>
              </invoice:each-client>
            </invoice:clients>
            <th>Rate</th>
            <th class="text-right px-6">Total</th>
          </tr>
        </thead>
        <!-- Body -->
        <tbody>
          <invoice:matrix>
            <matrix:each-row>
              <tr class="bg-white border-b ">
                <!-- Row: period -->
                <th class="px-6 py-4 font-mono font-medium text-sm text-gray-900 whitespace-nowrap">
                  <matrix:row />
                </th>
                <!-- Row: clients -->
                <matrix:cols>
                  <matrix:each-column>
                    <td>
                      <matrix:cell />
                    </td>
                  </matrix:each-column>
                </matrix:cols>
                <td>
                  <invoice:metadata>
                    <!-- DoNotFormat -->
                    <dollar /><value var="hourly-rate" />/hr
                    <!-- DoNotFormat -->
                  </invoice:metadata>
                </td>
                <td class="text-right px-6 font-mono">
                  <!-- DoNotFormat -->
                  <dollar /><matrix:row:total />
                  <!-- DoNotFormat -->
                </td>
              </tr>


            </matrix:each-row>
          </invoice:matrix>
          <tr>
            <th colspan="4" class="bg-gray-200"></th>
            <th class="px-6 py-4">Amount Owed</th>
            <td>
              <!-- DoNotFormat -->
              <span class="text-lg font-bold">USD <dollar /><invoice:matrix:total /></span>
              <!-- DoNotFormat -->
            </td>
          </tr>

        </tbody>
      </table>
    </div>

    <div class="text-center text-xl">
      <invoice:metadata>
        <snippet var="payment" />
      </invoice:metadata>
    </div>

  </div>
</body>

</html>