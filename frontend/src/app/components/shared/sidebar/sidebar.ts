import {Component} from '@angular/core';
import {CommonModule} from '@angular/common';

@Component({
  selector: 'app-sidebar',
  standalone: true,
  imports: [CommonModule],
  templateUrl: './sidebar.html',
  styleUrls: ['./sidebar.scss']
})
export class Sidebar {
  onlineFriends = [
    {id: 1, name: 'Nguyễn Văn A', avatar: 'assets/img/200.jpg', status: 'online'},
    {id: 2, name: 'Trần Thị Bảo Nhi', avatar: 'assets/img/200.jpg', status: 'busy'},
    {id: 3, name: 'Lê Văn Chiến', avatar: 'assets/img/200.jpg', status: 'online'},
  ];

  offlineFriends = [
    {id: 1, name: 'Phạm Thị Diễm My', avatar: 'assets/img/200.jpg', lastSeen: '2h trước'},
  ];
}
