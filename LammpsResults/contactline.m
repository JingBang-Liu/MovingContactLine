clear all
bins_data = matfile('densM.mat');
bins = bins_data.bins;

s = size(bins);
timemarks = s(1);
NBX = s(2);
NBY = s(3);
NBZ = s(4);

XD = [130,500];
YD = [0,121.83];
ZD = [11.79,98.0];

x = linspace(XD(1),XD(2),NBX);
y = linspace(YD(1),YD(2),NBY);
z = linspace(ZD(1),ZD(2),NBZ);
xyplane = permute(bins(2,:,:,1),[3,2,1,4]);

C = xyplane;

xSplit = diff(x)/2;                 % Find edge points
ySplit = diff(y)/2;
xEdges = [x(1)-xSplit(1) x(2:end)-xSplit x(end)+xSplit(end)];
yEdges = [y(1)-ySplit(1) y(2:end)-ySplit y(end)+ySplit(end)];
[XGrid, YGrid] = meshgrid(xEdges,yEdges);
YGrid = flipud(YGrid);              % To match expected behavior
C = [[C zeros(size(C,1),1)] ; zeros(1,size(C,2)+1)];% Last row/col ignored
pcolor(XGrid,YGrid,C)
% hold on                             % Plot original data points
% [X,Y] = meshgrid(x,y);
% Y = flipud(Y);
% plot(X,Y,'or')
xlabel('x');
ylabel('y');

frameN = timemarks

for i=1:frameN
    figure(1)
    set(gcf,'position',[10,10,570,417])
    C = permute(bins(i,:,:,1),[3,2,1,4]);
    C = [[C zeros(size(C,1),1)] ; zeros(1,size(C,2)+1)];% Last row/col ignored
    pcolor(XGrid,YGrid,C)
    F(i) = getframe(gcf);
    drawnow
    ax = gcf;
    ax.Units = 'pixels';
    fprintf('%d %d %d %d\n',ax.Position)
    s = size(F(i).cdata);
    fprintf('%d %d\n', s(2), s(1))
end

writerObj = VideoWriter('ContactLineBottom.avi');
writerObj.FrameRate = 10;
writerObj.Quality = 95;

open(writerObj);
for i=1:frameNf
    frame = F(i);
    writeVideo(writerObj, frame);
end
close(writerObj);
close(figure(1))
