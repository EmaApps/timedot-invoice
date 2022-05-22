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
  <style>
    @media print {
      body {
        background-color: white !important;
      }
    }
  </style>

</head>

<!-- DoNotFormat -->
<bind tag="theme"><invoice:metadata><value var="theme" /></invoice:metadata></bind>
<bind tag="dollar"><span class="text-${theme}-700" title="USD">$</span></bind>
<!-- DoNotFormat -->

<body class="overflow-y-scroll bg-gray-200 ">
  <div class="container max-w-screen-lg px-4 pt-4 pt-8 pb-8 mx-auto mt-8 bg-white">
    <header class="py-2 mb-4 text-4xl text-center text-gray-200 bg-gray-800 border-b-2 rounded">
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

    <div class="flex flex-row my-8">
      <section id="me" class="flex flex-col w-2/3 flex-start ">
        <div>Payable to:</div>
        <div class="mt-2 text-3xl font-bold">
          <invoice:metadata>
            <value var="name" />
          </invoice:metadata>
        </div>
        <address id="address" class="">
          <invoice:metadata>
            <with var="address">
              <div>
                <value />
              </div>
            </with>
          </invoice:metadata>
        </address>
        <div class="mt-4">
          Invoice date:
          <span class="font-bold text-${theme}-700">
            <invoice:number />
          </span>
        </div>

      </section>
      <section id="them" class="flex flex-col flex-start">
        <div>Client:</div>
        <div class="mt-2 text-3xl font-bold">
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
        <div class="mt-4 text-2xl font-bold">
          <invoice:metadata>
            <value var="payable-to.contact.name" />
          </invoice:metadata>
        </div>
        <div class="text-sm">
          <invoice:metadata>
            <with var="payable-to.contact">
              <a href="tel:${value:phone}">
                <value var="phone" />
              </a>
              /
              <a href="mailto:${value:email}">
                <value var="email" />
              </a>
            </with>
          </invoice:metadata>
        </div>


      </section>

    </div>

    <div class="shadow-md border-${theme}-700 border-2 sm:rounded-lg my-4">
      <table class="w-full text-left text-gray-700 table-auto ">
        <!-- Header -->
        <thead class="text-xs text-gray-100 bg-${theme}-700 uppercase bg-gray-50 ">
          <tr>
            <th></th>
            <invoice:clients>
              <invoice:each-client>
                <th class="py-3 pr-6 text-left">
                  <invoice:client />
                </th>
              </invoice:each-client>
            </invoice:clients>
            <th>Rate</th>
            <th class="px-6 text-right">Total</th>
          </tr>
        </thead>
        <!-- Body -->
        <tbody>
          <invoice:matrix>
            <matrix:each-row>
              <tr class="bg-white border-b ">
                <!-- Row: period -->
                <th class="px-6 py-4 font-mono text-sm font-medium text-gray-900 whitespace-nowrap">
                  <matrix:row />
                </th>
                <!-- Row: clients -->
                <matrix:cols>
                  <matrix:each-column>
                    <td class="text-left">
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
                <td class="px-6 font-mono text-lg text-right">
                  <!-- DoNotFormat -->
                  <dollar /><matrix:row:total />
                  <!-- DoNotFormat -->
                </td>
              </tr>

            </matrix:each-row>
          </invoice:matrix>
          <tr>
            <th class="bg-gray-200"></th>
            <invoice:clients>
              <invoice:each-client>
                <th class="bg-gray-200">
                </th>
              </invoice:each-client>
            </invoice:clients>
            <th class="px-6 py-4">Total</th>
            <td class="pr-6 font-mono text-lg font-bold text-right">
              <!-- DoNotFormat -->
              <dollar /><invoice:matrix:total />
              <!-- DoNotFormat -->
            </td>
          </tr>

        </tbody>
      </table>
    </div>


    <div class="text-xl text-center">
      <invoice:metadata>
        <snippet var="payment" />
      </invoice:metadata>
    </div>

    <div class="relative flex items-center py-5">
      <div class="flex-grow border-t border-gray-400"></div>
      <span class="flex-shrink mx-4 text-gray-400">Thank you for your business</span>
      <div class="flex-grow border-t border-gray-400"></div>
    </div>
  </div>
</body>

</html>