import {Component, OnInit} from '@angular/core';
import {CommonModule} from '@angular/common';
import Swal from 'sweetalert2';
import {UserResponse} from '../../../../responses/nguoidung/user-response';
import {Base} from '../../../base/base';
import {ResponseObject} from '../../../../responses/response-object';
import {UserSummaryResponse} from '../../../../responses/nguoidung/user-summary-response';
import {LichSuTranDauResponse} from '../../../../responses/trandau/lichsutrandau';


@Component({
  selector: 'app-user-detail',
  imports: [CommonModule],
  standalone: true,
  templateUrl: './user-detail.html',
  styleUrl: './user-detail.scss',
})
export class UserDetail extends Base implements OnInit {
  user: UserResponse | null = null;
  summary: UserSummaryResponse | null = null;
  battleHistory: LichSuTranDauResponse[] = [];

  selected_user_id: number | undefined;

  user_history_loading = false;
  user_history_items: LichSuTranDauResponse[] = [];
  user_history_page = 0;
  user_history_limit = 5;
  user_history_total_pages = 0;

  loading = false;


  ngOnInit(): void {
    const idParam = this.route.snapshot.paramMap.get('id');
    const userId = idParam ? +idParam : 0;
    this.selected_user_id = userId;
    this.loadUser(userId);
    this.loadSummary(userId);
  }

  loadUser(userId: number): void {
    this.loading = true;
    this.userService.getUserById(userId).subscribe({
      next: (res: ResponseObject<UserResponse>) => {
        console.log('User detail:', res.data);
        this.user = res.data || null;
        this.loading = false;
      },
      error: () => {
        this.loading = false;
        Swal.fire('Lỗi', 'Không thể tải thông tin người dùng', 'error').then(r => {
        });
      }
    });
  }

  loadSummary(userId: number): void {
    this.loading = true;
    this.userService.getUserSummary(userId).subscribe({
      next: (res: ResponseObject<UserSummaryResponse>) => {
        this.summary = res.data || null;
        this.loadUserHistory(0);
        this.loading = false;
      },
      error: () => {
        this.loading = false;
        Swal.fire('Lỗi', 'Không thể tải thông tin tổng quan người dùng', 'error');
      }
    });
  }


  loadUserHistory(page: number = 0) {
    this.tranDauService.getUserHistory(this.selected_user_id!, page, this.user_history_limit)
      .subscribe({
        next: (res) => {
          this.user_history_loading = false;
          const p = res.data!;
          this.user_history_items = p.items || [];
          this.battleHistory = this.user_history_items;
          console.log('User battle history:', this.battleHistory);
        },
        error: (err) => {
          this.user_history_loading = false;
        }
      });
  }

  goBack(): void {
    this.router.navigate(['/admin/users']).then(r => {
    });
  }
}
