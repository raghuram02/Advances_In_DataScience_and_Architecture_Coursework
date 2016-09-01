// This code requires the Nuget package Microsoft.AspNet.WebApi.Client to be installed.
// Instructions for doing this in Visual Studio:
// Tools -> Nuget Package Manager -> Package Manager Console
// Install-Package Microsoft.AspNet.WebApi.Client

using System;
using System.Collections.Generic;
using System.IO;
using System.Net.Http;
using System.Net.Http.Formatting;
using System.Net.Http.Headers;
using System.Text;
using System.Threading.Tasks;
using Newtonsoft.Json;
using WebApplication2_Clustering;

namespace CallRequestResponseService
{

    public class StringTable
    {
        public string[] ColumnNames { get; set; }
        public string[,] Values { get; set; }
    }

    class AzureMachineLearning2
    {

        public static string PredictPolarityClustering(PolarityClustering textPolarity)
        {
            using (var client = new HttpClient())
            {
                var scoreRequest = new
                {

                    Inputs = new Dictionary<string, StringTable>() {
                        {
                            "input1",
                            new StringTable()
                            {
                                ColumnNames = new string[] { "text", "emotion", "polarity", "pol"},
                                Values = new string[,] {  { textPolarity.text, textPolarity.emotion, textPolarity.polarity, textPolarity.pol.ToString()} }
                            }
                        },
                    },
                    GlobalParameters = new Dictionary<string, string>()
                    {
                    }
                };
                const string apiKey = "B3sIw+xt8Bjz2as6O/P9SHPVF5Xp6PYZUk2KjyUejzPxgRmUXUIQu0bF16ZeRJncOPeM6/Z+zOJU0CDcY4S77w=="; // Replace this with the API key for the web service
                client.DefaultRequestHeaders.Authorization = new AuthenticationHeaderValue("Bearer", apiKey);

                client.BaseAddress = new Uri("https://ussouthcentral.services.azureml.net/workspaces/b2c9e122af944685851e8622d46c7d1a/services/a568181412454f1fad8b7e498dd48a25/execute?api-version=2.0&details=true");




                HttpResponseMessage response = client.PostAsJsonAsync("", scoreRequest).Result;

                if (response.IsSuccessStatusCode)
                {
                    string jsonDocument = response.Content.ReadAsStringAsync().Result;

                    var responseBody = JsonConvert.DeserializeObject<RRSResponseObject>(jsonDocument);

                    return responseBody.Results.output1.value.Values[0][0];

                }
                else
                {
                    return "Error";
                }
            }
        }


        public class RRSResponseObject
        {
            public Results Results { get; set; }
        }

        public class Results
        {
            public Output1 output1 { get; set; }
        }

        public class Output1
        {
            public string type { get; set; }
            public Value value { get; set; }
        }

        public class Value
        {
            public string[] ColumnNames { get; set; }
            public string[] ColumnTypes { get; set; }
            public string[][] Values { get; set; }
        }

    }
}
