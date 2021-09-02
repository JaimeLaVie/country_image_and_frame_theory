""" 本程序筛选提及指定名词的英语推文，指定名词包括新疆棉花事件相关、立陶宛台湾时间相关词汇 """
import os
import jsonlines
import json
# from langdetect import detect    # 检测正确率惨不忍睹
import langid    # 其实不需要！每条推特都自带了语言标注！
import re

# 大小写无所谓，后面的程序中会全部lower()
keywords_x_1 = ['xinjiang', 'uyghur', 'uighur'] # 注意这里与R程序相比删除了china
keywords_x_2 = ['cotton', 'genocide', 'forced labor', 'forced labour']
keywords_l_1 = ['lithuania'] # 其实已经包括了立陶宛人、立陶宛的Lithuanian
keywords_l_2 = ['taiwan']
current_path = os.getcwd()

# paths = ['/intl_relations_20200304_0514', '/intl_relations_20200515_0604', '/intl_relations_20200605_0624', '/intl_relations_20200625_0714', '/intl_relations_20200715_0804']
# paths = ['intl_relations_20200304_0514']
# paths = ['intl_relations_20200515_0604']
# paths = ['intl_relations_20200605_0624']
# paths = ['intl_relations_20200625_0714']
# paths = ['intl_relations_20200715_0804']
# paths = ['intl_relations_20200805_0824']
# paths = ['intl_relations_20200825_0914']
# paths = ['intl_relations_20200915_1004']
# paths = ['intl_relations_20201005_1024']
# paths = ['intl_relations_20201025_1114']
# paths = ['intl_relations_20201115_1204']
# paths = ['intl_relations_20201205_1224']
# paths = ['intl_relations_20201225_0114'] √
# paths = ['intl_relations_20210115_0204'] √
# paths = ['intl_relations_20210205_0224'] √
# paths = ['intl_relations_20210225_0314'] √
# paths = ['intl_relations_20210315_0404'] √
# paths = ['intl_relations_20210405_0424'] √
# paths = ['intl_relations_20210425_0514'] √
# paths = ['intl_relations_20210515_0604'] √
# paths = ['intl_relations_20210605_0624'] √
# paths = ['intl_relations_20210625_0714'] √
# paths = ['intl_relations_20210715_0804'] √
# paths = ['intl_relations_20210805_0824'] √
paths = ['intl_relations_20210825_0914']

for path in paths:
    print (path)
    filedir = os.path.join(current_path, '../', path)
    filenames = os.listdir(filedir)
    targetdir_x = os.path.join(current_path, 'xinjiang_cotton')
    targetdir_l = os.path.join(current_path, 'lithuania_taiwan')

    for filename in filenames:
        print (filename)
        filepath = os.path.join(filedir, filename)
        # print (filepath)
        with open (filepath, 'r') as f:
            try:
                for tweets in jsonlines.Reader(f):
                    try:
                        text = tweets['text'].lower()
                        try:
                            # lang = str(langid.classify(text)[0])
                            lang = tweets["lang"]
                            if lang == 'en':
                                for wx1 in keywords_x_1:
                                    if re.findall(wx1.lower(), text) != []:
                                        for wx2 in keywords_x_2:
                                            if re.findall(wx2.lower(), text) != []:
                                                with open ('{}/{}.jsonl'.format(targetdir_x, filename.split('_')[1]), "a") as file:
                                                    file.write(json.dumps(tweets)+'\n')
                                for wl1 in keywords_l_1:
                                    if re.findall(wl1.lower(), text) != []:
                                        for wl2 in keywords_l_2:
                                            if re.findall(wl2.lower(), text) != []:
                                                with open ('{}/{}.jsonl'.format(targetdir_l, filename.split('_')[1]), "a") as file:
                                                    file.write(json.dumps(tweets)+'\n')
                        except:
                            print('ERROR!')
                            pass
                            # with open ('{}/not_a_language_{}.jsonl'.format(targetdir, country_names), "a") as file:
                            #     file.write(json.dumps(tweets)+'\n')
                            # print ('3')
                    except:
                        print (filename)
                        try:
                            print (tweets['id'])
                        except:
                            try:
                                print (tweets['limit'])
                            except:
                                print (tweets)
            except:
                print ("文件内没有内容！文件名：" + filename)
                continue