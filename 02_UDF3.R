# �ʿ��� ��Ű�� load
library(KoNLP)      # to use useSejongDic(), extractNoun(), useNIADic(), ...  
library(wordcloud)  # to use wordcloud()

# KoNLP�� �ִ� ���������� ����ϱ� ���� �Լ�
useSejongDic()


### (1) ������ 
make_dataset <- function() {
  text<- "�ؽ�Ʈ���̴��� �ڿ��� (Natural Language)�� ������ ������������ (unstructured data)���� ���� �Ǵ� ���踦 �����Ͽ� 
          �ǹ��ִ� ������ ã�Ƴ��� �������, ��ǻ�Ͱ� ������� ���ϴ� �� ������ �� �ִ� �ڿ��� ó�� (Natural Language 
          Processing)�� ������� �� ����̴�. Ʈ����, ���̽��ϰ� ���� �Ҽ� �̵��� ����Ǵ� �����ʹ� �������������̱� 
          ������ �ؽ�Ʈ���̴��� �̿��Ͽ� �м��� �� �ִ�.
          �ؽ�Ʈ���̴��� �� �״�� �ؽ�Ʈ ������ �����������Ϳ� ���̴� ����� ������ ���̴�. ��, �ؽ�Ʈ�� ��Ÿ���� �ܾ 
          ����, �����ϰ�, Ư�� �ܾ��� ������ ���� �ľ��Ͽ� �ܾ�� ���� ���踦 �����ϴ� ����̴�. 
          �����͸��̴� (data mining)�� ��Ը� DB�� ����� ����ȭ�� �����ͷκ��� ������ ã�Ƴ��� ����̶�� �ؽ�Ʈ���̴��� 
          ������ȭ�� �ؽ�Ʈ �������� ������ ã�Ƴ��� ����̶� �� �� �ִ�.
          �׸� 2.1��  �����͸��̴� �� �ؽ�Ʈ���̴��� ���踦 ��Ÿ���� �׸��̴�."
  
  return (text)
}

### (2) �������� �� ����ó��
kword_pre = function(text) {
  nouns = extractNoun(text) #���縸 ����
  
  nouns <- nouns[nchar(nouns)>=2]   #���� �߿��� 2���� �̻� �����
  
  nouns <- gsub("�ؽ�Ʈ���̴�.*","�ؽ�Ʈ���̴�", nouns)
  nouns <- gsub("�����͸��̴�.*","�����͸��̴�", nouns)
  
  return (nouns)
}

### (3) ���� Ŭ����
wordcl = function(nouns){
  # �� ������ �м�
  wordFreq <-table(nouns)    #�� ���
  
  
  # ���� Ŭ����
  pal <- brewer.pal(6, "Dark2")
  windowsFonts(malgun=windowsFont("���� ����"))
  
  wordcloud(words=names(wordFreq), freq=wordFreq, colors=pal, min.freq=1, random.order=F, family="malgun")  
}

text = make_dataset()
nouns = kword_pre(text)
wordcl(nouns)