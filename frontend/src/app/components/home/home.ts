import {Component, OnInit} from '@angular/core';
import {CommonModule} from '@angular/common';
import {ResponseObject} from '../../responses/response-object';
import {TranDauResponse} from '../../responses/trandau/trandau-response';
import {BoCauHoiResponse} from '../../responses/bocauhoi/bocauhoi-response';
import {PageResponse} from '../../responses/page-response';
import {UserResponse} from '../../responses/nguoidung/user-response';
import {Base} from '../base/base';

@Component({
  selector: 'app-home',
  standalone: true,
  imports: [CommonModule],
  templateUrl: './home.html',
  styleUrl: './home.scss'
})
export class Home extends Base implements OnInit {
  pendingBattles: TranDauResponse[] = [];
  featuredQuizzes: BoCauHoiResponse[] = [];
  user?: UserResponse | null;


  ngOnInit() {
    this.loadPendingBattles();
    this.loadFeaturedQuizzes();
    this.user = this.userService.getUserResponseFromLocalStorage();
  }

  loadPendingBattles() {
    this.tranDauService.getPendingBattles(0, 5).subscribe({
      next: (res: ResponseObject<PageResponse<TranDauResponse>>) => {
        this.pendingBattles = res.data?.items ?? [];
      },
      error: (err) => console.error('❌ Lỗi khi tải danh sách bộ câu hỏi:', err)
    });
  }

  loadFeaturedQuizzes() {
    this.bocauHoiService.getFeatured(3).subscribe({
      next: (res: ResponseObject<PageResponse<BoCauHoiResponse>>) => {
        this.featuredQuizzes = res.data?.items ?? [];
      },
      error: (err) => console.error('❌ Lỗi khi tải danh sách bộ câu hỏi:', err)
    });
  }

  navigateQuiz() {
    this.router.navigate(['/bo-cau-hoi/danh-sach-bo-cau-hoi']).then(r => {});
  }

  navigateBattle() {
    this.router.navigate(['/tran-dau/pending']).then(r => {});
  }

  navigateHistory() {
    this.router.navigate(['/tran-dau/lich-su-tran-dau']).then(r => {});
  }
}
