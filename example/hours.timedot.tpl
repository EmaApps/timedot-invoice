<!DOCTYPE HTML>
<html lang="en">

<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <title>
    Invoice
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

    <p>TODO: Contact</p>
    <div class="relative overflow-x-auto shadow-md sm:rounded-lg">
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
            <th>Total</th>
          </tr>
        </thead>
        <!-- Body -->
        <tbody>
          <invoice:matrix>
            <matrix:each-row>
              <tr class="bg-white border-b ">
                <!-- Row: period -->
                <th class="px-6 py-4 font-medium text-gray-900 whitespace-nowrap">
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
                    <dollar /><value var="hourly_rate" />/hr
                    <!-- DoNotFormat -->
                  </invoice:metadata>
                </td>
                <td>
                  <!-- DoNotFormat -->
                  USD <dollar /><matrix:row:total />
                    <!-- DoNotFormat -->
                </td>
              </tr>


            </matrix:each-row>
          </invoice:matrix>
          <tr>
            <th colspan="5" class="text-right px-6 py-4">Total</th>
            <td>
              <!-- DoNotFormat -->
              <span class="text-lg font-bold">USD <dollar /><invoice:matrix:total /></span>
              <!-- DoNotFormat -->
            </td>
          </tr>

        </tbody>
      </table>
    </div>

    <p>TODO: Payment info</p>

  </div>
</body>

</html>