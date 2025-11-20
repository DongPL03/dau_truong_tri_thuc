import {Component, computed, OnInit, signal} from '@angular/core';
import {CommonModule} from '@angular/common';
import {Base} from '../../base/base';
import {ResponseObject} from '../../../responses/response-object';
import {
  LichSuTranDauDetailResponse,
  LichSuTranDauQuestionResponse
} from '../../../responses/trandau/lich-su-tran-dau-detail-response';
import {FinishedPlayer} from '../../../responses/trandau/finished-player';
import { environment } from 'src/app/environments/environment';

@Component({
  selector: 'app-lich-su-tran-dau-detail',
  standalone: true,
  imports: [CommonModule],
  templateUrl: './lich-su-tran-dau-detail.html',
  styleUrl: './lich-su-tran-dau-detail.scss'
})
export class LichSuTranDauDetail extends Base implements OnInit {

  loading = signal<boolean>(true);
  data = signal<LichSuTranDauDetailResponse | null>(null);

  readonly questionOptions: ('A' | 'B' | 'C' | 'D')[] = ['A', 'B', 'C', 'D'];

  protected readonly environment = environment;

  myId = this.userService.getUserId();

  isWinner = computed(() => {
    const d = this.data();
    if (!d) return false;
    const winner = d.leaderboard.find(p => p.xep_hang === 1);
    return !!winner && winner.user_id === this.myId;
  });

  ngOnInit(): void {
    const tranDauId = Number(this.route.snapshot.paramMap.get('id'));
    if (!tranDauId) return;

    this.tranDauService.getMyHistoryDetail(tranDauId).subscribe({
      next: (res: ResponseObject<LichSuTranDauDetailResponse>) => {
        this.data.set(res.data!);
        this.loading.set(false);
      },
      error: () => {
        this.loading.set(false);
      }
    });
  }

  seconds(ms: number): string {
    return (ms / 1000).toFixed(1) + 's';
  }

  choiceText(q: LichSuTranDauQuestionResponse, key: 'A' | 'B' | 'C' | 'D'): string {
    switch (key) {
      case 'A':
        return q.lua_chon_a;
      case 'B':
        return q.lua_chon_b;
      case 'C':
        return q.lua_chon_c;
      case 'D':
        return q.lua_chon_d;
    }
  }

  isMyRow(p: FinishedPlayer): boolean {
    return p.user_id === this.myId;
  }

  backToHistory() {
    this.router.navigateByUrl('/battle/history').then();
  }

  practiceSet() {
    const d = this.data();
    if (!d) return;
    this.router.navigate(['/bo-cau-hoi/detail', d.tran_dau_id]).then(); // hoặc bo_cau_hoi_id nếu bạn muốn
  }
}
