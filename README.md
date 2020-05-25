# Zimnat-Insurance-Challenge
1st Place Solution

The data describes 51,685 life assurance policies, each identified by a unique Policy ID. Each year, some policies lapse as clients change jobs, move countries etc. The information on these policies are in multiple files.

Numerical quantities have been transformed, and many categories have been assigned unique IDs in place of the original text.

The objective of this hackathon is to develop a predictive model that determines the likelihood for a customer to churn - to seek an alternative insurer or simply stop paying for insurance altogether.

sample_submission.csv contains rows for the remaining Policy IDs (with ‘?’s in TRAIN). You must predict which of these policies marked with '?' will lapse in 2020.

Files available for download
client_data.csv - Contains some personal information on the principal member, such as location, branch and agent code, age etc.
payment_history.csv - Contains payment history up to the end of 2018 tied to Policy ID. Payments made in 2019 are not provided.
policy_data.csv - Describes the policies themselves. There may be multiple rows for each Policy ID since policies can cover more than one person.
train.csv - contains a list of all the policies. Policies that lapsed in 2017, 2018 or 2019 are identified with a 1 in the ‘Lapse’ column, and the year is provided. The policies with a '?' in the 'Lapse' and 'Lapse Year' column are the policies that remained and had not lapsed as of the end of 2019. You must estimate the likelihood that these policies lapsed or not in 2020.
sample_submission.csv - is an example of what your submission should look like. The order of the rows does not matter but the name of the ID must be correct.
variable_defintions.txt - definitions of the variables

https://zindi.africa/hackathons/the-zimnat-insurance-assurance-challenge/data
