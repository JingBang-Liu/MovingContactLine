clear all
% fileID = fopen('bins_dens.dat','r');
% formatSpec = '%f';
% A = fscanf(fileID,formatSpec);




NBX = 37;
NBY = 10;
NBZ = 10;

XD = [130,500];
YD = [0,121.83];
ZD = [11.79,98.0];

timemarks = 2000;

bins = zeros(timemarks,NBX,NBY,NBZ);

filename = 'bins_dens.dat';
% delimiterIn = '';
% headerlinesIn = 3;
% A = importdata(filename,delimiterIn,headerlinesIn);

file = fileread(filename);
lines = regexp(file,'\n','split');
clear file
headerlines = 3;
block = 3701;
k = headerlines + 1 + (5000-timemarks)*block;
i = 1;
t = 1;
while k < length(lines)-block
    for j=1:3700
        tempZ = mod(j-1,10) + 1;
        tempY = mod((j-tempZ)/10,10) + 1;
        tempX = (j-tempZ-10*(tempY-1))/100 + 1;
        tempVec = cell2mat(textscan(char(lines(k+j)),'%f'));
        bins(t,tempX,tempY,tempZ) = tempVec(5);
    end
    t = t + 1
    k = k + block;
end

save('densM.mat','bins')

