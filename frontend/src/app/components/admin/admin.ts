// src/app/components/admin/admin.component.ts
import {Component, OnInit} from '@angular/core';
import {CommonModule} from '@angular/common';
import {RouterLink, RouterLinkActive, RouterOutlet} from '@angular/router';
import {Base} from '../base/base';
import {UserResponse} from '../../responses/nguoidung/user-response';

@Component({
  selector: 'app-admin-layout',
  standalone: true,
  imports: [CommonModule, RouterOutlet, RouterLink, RouterLinkActive],
  templateUrl: './admin.html',
  styleUrls: ['./admin.scss']
})
export class Admin extends Base implements OnInit {

  user?: UserResponse | null = null;

  ngOnInit(): void {
    this.user = this.userService.getUserResponseFromLocalStorage();
  }

  get displayName(): string {
    return (
      this.user?.ten_hien_thi ||
      this.user?.ho_ten ||
      this.user?.email ||
      'Admin'
    );
  }

  logout(): void {
    this.tokenService.clear();
    this.userService.removeUserFromLocalStorage();
    this.router.navigate(['/login']).then(() => {
      setTimeout(() => location.reload(), 200);
    });
  }

  goTo(path: string): void {
    // path kiểu 'dashboard', 'bo-cau-hoi', 'users', ...
    this.router.navigate(['/admin', path]).then(r => {
    });
  }
}
