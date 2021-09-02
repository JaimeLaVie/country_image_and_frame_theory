# country_image_and_frame_theory
此库包含了本研究用到的所有程序和数据（原始数据因数据量过大，只提供少量案例，结果数据均完整）。

文件夹news_data包含了获取新闻数据的所有程序，其中GDELT_download_clean.R从GDELT数据集里下载完整数据并保存到gdelt_data文件夹，gdelt_get_web_content_xxx.R分别从gdelt_data文件夹中的每一份文件里寻找BBC、AP、RT的新闻，并通过其网址下载所有新闻文本，gdelt_select_news_data_xinjiang.R对指定新闻来源和时间段的新闻文本进行筛选，以获得本研究所需要的新闻文本。所有筛选后获得的文本均存放在news/selected文件夹里。

文件夹twitter_data包含了筛选推特数据的所有程序。通过推特开发者账号获得的所有数据，经过filter.py筛选后按主题保存到xinjiang_cotton和lithuania_taiwan两个文件夹中，再由combine.py将所有重复（转发）推特提取出来保存在对应的两个xlsx文件中。