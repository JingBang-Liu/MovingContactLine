clear all
filename = '../analysis/contactline_matrix.dat';
fileID = fopen(filename,'r');
formatSpec = '%f';
A = fscanf(fileID,formatSpec);

timemarks = 251;
NBX = 92;
NBY = 30;

XD = [130,500];
YD = [0,121.83];

x = linspace(XD(1),XD(2),NBX);
y = linspace(YD(1),YD(2),NBY);
bins = reshape(A,[30,92,251]);

xSplit = diff(x)/2;                 % Find edge points
ySplit = diff(y)/2;
xEdges = [x(1)-xSplit(1) x(2:end)-xSplit x(end)+xSplit(end)];
yEdges = [y(1)-ySplit(1) y(2:end)-ySplit y(end)+ySplit(end)];
[XGrid, YGrid] = meshgrid(xEdges,yEdges);
YGrid = flipud(YGrid);              % To match expected behavior
C = bins(:,:,3);
C = [[C zeros(size(C,1),1)] ; zeros(1,size(C,2)+1)];% Last row/col ignored
pcolor(XGrid,YGrid,C)
colorbar;
% hold on                             % Plot original data points
% [X,Y] = meshgrid(x,y);
% Y = flipud(Y);
% plot(X,Y,'or')
xlabel('x');
ylabel('y');

frameN = timemarks;

% for i=1:frameN
%     figure(1)
%     set(gcf,'position',[10,10,570,417])
%     C = bins(:,:,i)
%     C = [[C zeros(size(C,1),1)] ; zeros(1,size(C,2)+1)];% Last row/col ignored
%     pcolor(XGrid,YGrid,C)
%     F(i) = getframe(gcf);
%     drawnow
%     ax = gcf;
%     ax.Units = 'pixels';
%     fprintf('%d %d %d %d\n',ax.Position)
%     s = size(F(i).cdata);
%     fprintf('%d %d\n', s(2), s(1))
% end
% 
% writerObj = VideoWriter('ContactLineBottomFine.avi');
% writerObj.FrameRate = 10;
% writerObj.Quality = 95;
% 
% open(writerObj);
% for i=1:frameN
%     frame = F(i);
%     writeVideo(writerObj, frame);
% end
% close(writerObj);
% close(figure(1))

binsX = zeros(timemarks,NBX);
% for i=1,timemarks
%     binsX(i,:) = sum(bins(:,:,i));
% end
for i=1:timemarks
    binsX(i,:) = permute(sum(bins(:,:,i)),[2,1]);
end
% chunkdens = mean(reshape(binsX(1:251,30:55),[1,251*26]))
% chunkdens = mean(binsX(1:251,30:55),'all')
chunkdens = mean(permute(binsX(1,30:55),[2,1]));
figure(2)
plot(x,binsX(2,:))
xlabel('x');
ylabel('N');
title('timestep 50');


